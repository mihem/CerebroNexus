/* Compact Linked views binary transport. */
(function () {
  'use strict';

  function unpackVector(buffer, dataStart, value) {
    var offset = Number(value.offset);
    var bytes = Number(value.bytes);
    var start = dataStart + offset;
    var end = start + bytes;
    if (offset < 0 || bytes < 0 || end > buffer.byteLength) {
      throw new Error('invalid Linked views wire vector');
    }
    var source = buffer.slice(start, end);
    if (value.__cv_wire__ === 'json') {
      return JSON.parse(new window.TextDecoder().decode(source));
    }
    var result = value.__cv_wire__ === 'f32'
      ? new Float32Array(source)
      : value.__cv_wire__ === 'f64'
        ? new Float64Array(source)
        : value.__cv_wire__ === 'i8'
          ? new Int8Array(source)
          : value.__cv_wire__ === 'i16'
            ? new Int16Array(source)
            : value.__cv_wire__ === 'i32' ? new Int32Array(source) : null;
    if (!result) {
      throw new Error('unknown Linked views wire vector');
    }
    if (result.length !== Number(value.length)) {
      throw new Error('invalid Linked views wire vector');
    }
    for (var index = 0; index < result.length; index++) {
      if (Number.isNaN(result[index]) || result[index] === -2147483648) {
        return Array.from(result, function (item) {
          return Number.isNaN(item) || item === -2147483648 ? null : item;
        });
      }
    }
    return result;
  }

  function unpackValue(buffer, dataStart, value) {
    if (!value || typeof value !== 'object') return value;
    if (typeof value.__cv_wire__ === 'string') {
      return unpackVector(buffer, dataStart, value);
    }
    var keys = Object.keys(value);
    keys.forEach(function (key) {
      value[key] = unpackValue(buffer, dataStart, value[key]);
    });
    return value;
  }

  function unpack(buffer) {
    if (!(buffer instanceof ArrayBuffer) || buffer.byteLength < 4) {
      throw new Error('invalid Linked views binary message');
    }
    var headerLength = new DataView(buffer, 0, 4).getUint32(0, true);
    var dataStart = 4 + headerLength + ((4 - headerLength % 4) % 4);
    if (dataStart > buffer.byteLength) {
      throw new Error('invalid Linked views binary header');
    }
    var header = new window.TextDecoder().decode(
      new Uint8Array(buffer, 4, headerLength)
    );
    return unpackValue(buffer, dataStart, JSON.parse(header));
  }

  function unpackCells(buffer) {
    if (!(buffer instanceof ArrayBuffer) || buffer.byteLength < 4) {
      throw new Error('invalid Linked views cell message');
    }
    var datasetLength = new DataView(buffer, 0, 4).getUint32(0, true);
    if (4 + datasetLength > buffer.byteLength) {
      throw new Error('invalid Linked views cell header');
    }
    var decoder = new window.TextDecoder();
    return {
      dataset_id: decoder.decode(new Uint8Array(buffer, 4, datasetLength)),
      cells: JSON.parse(decoder.decode(
        new Uint8Array(buffer, 4 + datasetLength)
      ))
    };
  }

  window.CBViewWire = Object.freeze({
    supported: typeof window.ArrayBuffer === 'function' &&
      typeof window.TextDecoder === 'function',
    unpack: unpack,
    unpackCells: unpackCells
  });
})();
