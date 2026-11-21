"""Run with Python + numpy/tifffile/imagecodecs/Pillow; no real data or network."""

import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest import mock

import numpy as np
from PIL import Image
import tifffile


SCRIPT = Path(__file__).resolve().parents[2] / "data-raw/large-demo/prepare_xenium_prime_image.py"
spec = importlib.util.spec_from_file_location("prime_image", SCRIPT)
prime = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prime)


class PrimeImageTests(unittest.TestCase):
    def test_windows_handle_zero_and_constant_channels(self):
        self.assertEqual(prime.display_window(np.zeros((4, 4))), (0, 1))
        self.assertEqual(prime.display_window(np.full((4, 4), 7)), (0, 7))

    def test_four_stains_and_physical_bounds_survive_downsampling(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "image.ome.tif"
            image = np.zeros((4, 32, 48), dtype=np.uint16)
            # Four distinct landmarks; each should have its own stain colour.
            for c in range(4):
                image[c, 4:12, c * 12 : (c + 1) * 12] = 1000
            metadata = {
                "axes": "CYX",
                "Channel": {"Name": [name for name, _ in prime.CHANNELS]},
                "PhysicalSizeX": 0.2125,
                "PhysicalSizeY": 0.2125,
                "PhysicalSizeXUnit": "µm",
                "PhysicalSizeYUnit": "µm",
            }
            with tifffile.TiffWriter(source, ome=True) as writer:
                writer.write(image, metadata=metadata, photometric="minisblack", subifds=1)
                writer.write(image[:, ::2, ::2], subfiletype=1, photometric="minisblack")
            png, info = root / "image.png", root / "image.json"
            result = prime.prepare(source, png, info, max_size=24)
            self.assertEqual(result["sourceSize"], [48, 32])
            self.assertEqual(result["displaySize"], [24, 16])
            self.assertEqual(result["bounds"], {
                "xmin": 0, "xmax": 10.2, "ymin": -6.8, "ymax": 0,
            })
            rendered = np.asarray(Image.open(png))
            self.assertEqual(rendered.shape, (16, 24, 3))
            for c, (_, colour) in enumerate(prime.CHANNELS):
                expected = [int(colour[i:i+2], 16) for i in (1, 3, 5)]
                np.testing.assert_array_equal(rendered[3, 6*c+2], expected)
            self.assertEqual(json.loads(info.read_text()), result)
            # Resizing to a non-pyramid dimension must keep the same extent.
            resized = prime.prepare(source, png, info, max_size=17)
            self.assertEqual(max(resized["displaySize"]), 17)
            self.assertEqual(resized["bounds"], result["bounds"])

    def test_single_channel_is_not_misrepresented_as_four_stains(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "dapi.ome.tif"
            tifffile.imwrite(
                source, np.ones((8, 8), dtype=np.uint16),
                metadata={"axes": "YX", "Channel": {"Name": ["DAPI"]}},
                photometric="minisblack",
            )
            with self.assertRaisesRegex(ValueError, "Unexpected Prime 5K morphology channels"):
                prime.prepare(source, root / "image.png", root / "image.json")
            self.assertFalse((root / "image.png").exists())

    def test_split_ome_uses_each_tiffs_pyramid_and_requires_all_peers(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            files = [root / f"morphology_focus_{c:04d}.ome.tif" for c in range(4)]
            channels = "".join(
                f'<Channel ID="Channel:{c}" Name="{name}" SamplesPerPixel="1"/>'
                for c, (name, _) in enumerate(prime.CHANNELS)
            )
            refs = "".join(
                f'<TiffData FirstC="{c}" PlaneCount="1"><UUID FileName="{file.name}">'
                f'urn:uuid:00000000-0000-0000-0000-00000000000{c}</UUID></TiffData>'
                for c, file in enumerate(files)
            )
            xml = (
                '<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">'
                '<Image ID="Image:0"><Pixels ID="Pixels:0" DimensionOrder="XYZCT" '
                'Type="uint16" SizeX="48" SizeY="32" SizeC="4" SizeZ="1" SizeT="1" '
                'PhysicalSizeX="0.2125" PhysicalSizeY="0.2125" '
                'PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm">'
                f'{channels}{refs}</Pixels></Image></OME>'
            )
            for c, file in enumerate(files):
                plane = np.zeros((32, 48), dtype=np.uint16)
                plane[4:12, c*12:(c+1)*12] = 1000
                with tifffile.TiffWriter(file, ome=False) as writer:
                    writer.write(
                        plane, description=xml.encode() if c == 0 else None,
                        metadata=None, photometric="minisblack", subifds=1,
                    )
                    writer.write(plane[::2, ::2], subfiletype=1, photometric="minisblack")
            png, info = root / "image.png", root / "image.json"
            read = tifffile.TiffPageSeries.asarray
            decoded = []

            def read_coarse(series, *args, **kwargs):
                decoded.append(series.shape)
                self.assertEqual(series.shape, (16, 24))
                return read(series, *args, **kwargs)

            with mock.patch.object(tifffile.TiffPageSeries, "asarray", read_coarse):
                result = prime.prepare(files[0], png, info, max_size=24)
            self.assertEqual(decoded, [(16, 24)] * 4)
            self.assertEqual(result["displaySize"], [24, 16])
            rendered = np.asarray(Image.open(png))
            for c, (_, colour) in enumerate(prime.CHANNELS):
                np.testing.assert_array_equal(
                    rendered[3, 6*c+2],
                    [int(colour[i:i+2], 16) for i in (1, 3, 5)],
                )
            files[3].unlink()
            with self.assertRaises(FileNotFoundError):
                prime.prepare(files[0], root / "missing.png", info, max_size=24)
            self.assertFalse((root / "missing.png").exists())


if __name__ == "__main__":
    unittest.main()
