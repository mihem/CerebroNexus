"""Prepare the Prime 5K demo's fixed-colour PNG, not a general image importer.

Build-time dependencies: numpy, tifffile, imagecodecs (JPEG2000), Pillow.
The four morphology_focus TIFFs must remain together: their OME metadata
describes one four-channel image spread over four files.
"""

import argparse
import json
from pathlib import Path
import xml.etree.ElementTree as ET

import numpy as np
from PIL import Image
import tifffile


CHANNELS = [
    ("DAPI", "#4c8dff"),
    ("ATP1A1/CD45/E-Cadherin", "#3fd14a"),
    ("18S", "#ff4fd8"),
    ("alphaSMA/Vimentin", "#ffd23f"),
]


def display_window(channel):
    """Stretch measured signal, not the full uint16 range."""
    step = max(1, int(np.sqrt(channel.size / 4_000_000)))
    values = channel[::step, ::step].ravel()
    values = values[np.isfinite(values) & (values > 0)]
    if not values.size:
        return 0.0, 1.0
    lo, hi = (float(x) for x in np.percentile(values, [0.5, 99.9]))
    return (lo, hi) if hi > lo else (0.0, max(hi, 1.0))


def read_level(series, max_size):
    level = series.levels[0]
    for candidate in series.levels:
        side = max(candidate.shape[candidate.axes.index(a)] for a in "YX")
        if side >= max_size:
            level = candidate
    return level.asarray(maxworkers=4), level.axes


def prepare(source, output, metadata, max_size=4096):
    if max_size < 1:
        raise ValueError("max_size must be positive")
    with tifffile.TiffFile(source) as tif:
        root = ET.fromstring(tif.ome_metadata)
        pixels = root.find("{*}Image/{*}Pixels")
        names = [ch.attrib["Name"] for ch in pixels.findall("{*}Channel")]
        expected = [name.casefold() for name, _ in CHANNELS]
        if [name.casefold() for name in names] != expected:
            raise ValueError(f"Unexpected Prime 5K morphology channels: {names}")
        sx = float(pixels.attrib["PhysicalSizeX"])
        sy = float(pixels.attrib["PhysicalSizeY"])
        for axis in ("X", "Y"):
            if pixels.attrib.get(f"PhysicalSize{axis}Unit", "µm") != "µm":
                raise ValueError("The morphology pixel size must be in micrometres")
        if not (np.isfinite(sx) and np.isfinite(sy) and sx > 0 and sy > 0):
            raise ValueError("The morphology pixel size must be finite and positive")
        width, height = int(pixels.attrib["SizeX"]), int(pixels.attrib["SizeY"])
        refs = {}
        for data in pixels.findall("{*}TiffData"):
            uuid = data.find("{*}UUID")
            if uuid is not None and "FileName" in uuid.attrib:
                refs[int(data.attrib.get("FirstC", 0))] = uuid.attrib["FileName"]
        if len(set(refs.values())) > 1:
            # tifffile cannot expose multi-file OME pyramids as one series:
            # doing so silently falls back to decoding all full-resolution
            # planes. Read each TIFF's native SubIFD pyramid instead.
            if set(refs) != set(range(4)):
                raise ValueError("All four morphology TIFF references are required")
            planes = []
            for c in range(4):
                filename = refs[c]
                if Path(filename).name != filename:
                    raise ValueError("Morphology TIFF peers must be in the same folder")
                with tifffile.TiffFile(Path(source).with_name(filename), is_ome=False) as peer:
                    series = peer.series[0]
                    if series.axes != "YX" or series.shape != (height, width):
                        raise ValueError("Morphology TIFF planes must share the native extent")
                    plane, _axes = read_level(series, max_size)
                    planes.append(plane)
            image, axes = np.stack(planes), "CYX"
        else:
            image, axes = read_level(tif.series[0], max_size)
        for i in reversed(range(image.ndim)):
            if axes[i] not in "CYX":
                if image.shape[i] != 1:
                    raise ValueError(f"Unexpected morphology axis: {axes[i]}")
                image = np.take(image, 0, axis=i)
                axes = axes[:i] + axes[i + 1 :]
        image = np.transpose(image, [axes.index(a) for a in "CYX"])
        if image.shape[0] != 4:
            raise ValueError("All four morphology channels are required")

    rgb = np.zeros((*image.shape[1:], 3), dtype=np.float32)
    channels = []
    for plane, (name, colour) in zip(image, CHANNELS, strict=True):
        lo, hi = display_window(plane)
        signal = np.clip((plane.astype(np.float32) - lo) / (hi - lo), 0, 1)
        weights = np.array([int(colour[i : i + 2], 16) / 255 for i in (1, 3, 5)])
        for c in range(3):
            rgb[:, :, c] += signal * weights[c]
        channels.append({"name": name, "color": colour, "window": [lo, hi]})
    png = Image.fromarray(np.round(np.clip(rgb, 0, 1) * 255).astype(np.uint8))
    if max(png.size) > max_size:
        ratio = max_size / max(png.size)
        png = png.resize(
            tuple(max(1, round(n * ratio)) for n in png.size),
            Image.Resampling.LANCZOS,
        )
    png.save(output)
    # Cell coordinates are (x_centroid, -y_centroid), so the native image's
    # top row is y=0 and its bottom edge is negative. No manual offset or flip.
    info = {
        "sourceSize": [width, height],
        "pixelSize": [sx, sy],
        "displaySize": list(png.size),
        "bounds": {"xmin": 0, "xmax": width * sx, "ymin": -height * sy, "ymax": 0},
        "channels": channels,
    }
    Path(metadata).write_text(json.dumps(info, indent=2) + "\n")
    return info


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source")
    parser.add_argument("output")
    parser.add_argument("metadata")
    parser.add_argument("--max-size", type=int, default=4096)
    args = parser.parse_args()
    prepare(args.source, args.output, args.metadata, args.max_size)
