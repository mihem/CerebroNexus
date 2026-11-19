/* Pure background-image selection and alignment state, shared across views. */
(function () {
  'use strict';
  var IMG_NONE = '__none__';
  function spatialImages(sp) {
    if (!sp) return [];
    if (sp.images && sp.images.length) return sp.images;
    // Multi-section bundles keep the image reference on each sample rather than
    // duplicating the opening sample's image data at the space level.
    // Before the picker has changed section, `_sampleName` is unset and the
    // opening sample is the active one.
    var samples = sp.samples || [];
    var name = sp._sampleName || (samples[0] && samples[0].name);
    var sample = samples.filter(function (s) { return s.name === name; })[0];
    return (sample && sample.images) || [];
  }
  function currentImage(sp) {
    var list = spatialImages(sp);
    if (!list.length) return null;
    var wanted = sp && sp._customImageId;
    if (wanted === IMG_NONE) return null;
    for (var i = 0; i < list.length; i++) {
      if (list[i].id === wanted) return list[i];
    }
    return list[0];
  }
  function spatialName(sp) {
    return sp && (sp._sampleName ||
      (sp.samples && sp.samples[0] && sp.samples[0].name) || sp.id);
  }
  // Visible labels are not state identities: a FOV may itself be named
  // "Trekker", so modality plus stable space id namespaces its background.
  function backgroundStateKey(sp) {
    if (!sp) return null;
    if (sp._spatialSample) return 'fov:' + (sp.id || spatialName(sp));
    return 'space:' + (sp.id || 'unknown') + ':' + spatialName(sp);
  }
  // The key a calibration is stored under. Both halves matter: the same image
  // against a different section is a different alignment problem.
  function imgKey(sp, img) {
    if (!sp || !img) return null;
    // The section name has to resolve the same way BEFORE the first switch as
    // after it. `_sampleName` is only set when the picker is used, so falling
    // back to the space id gave the opening section one key and the same
    // section a different one on return -- and the alignment stored under the
    // first was never found again.
    return backgroundStateKey(sp) + '|' + (img.id || 'image');
  }
  function presetState(img) {
    var pr = (img && img.preset) || {};
    return {
      show: true,
      opacity: (pr.opacity != null ? pr.opacity : 1),
      offsetX: (pr.offsetX != null ? pr.offsetX : 0),
      offsetY: (pr.offsetY != null ? pr.offsetY : 0),
      scaleX: (pr.scaleX != null ? pr.scaleX : 1),
      scaleY: (pr.scaleY != null ? pr.scaleY : 1),
      flipX: !!pr.flipX, flipY: !!pr.flipY,
      rotate: (pr.rotation != null ? pr.rotation : 0),
      // Part of the state, not of the controls: unlocking while X and Y happen
      // to be equal is a decision, and reading the lock back off the checkbox
      // silently re-made it as "locked" on the next visit.
      lock: (pr.scaleX == null || pr.scaleY == null || pr.scaleX === pr.scaleY)
    };
  }
  function imageRenderState(img, state) {
    var pr = (img && img.preset) || {};
    if (!pr.geometryBaked) return state;
    var baseScaleX = Number(pr.scaleX) || 1;
    var baseScaleY = Number(pr.scaleY) || baseScaleX;
    return {
      show: state.show,
      opacity: state.opacity,
      offsetX: state.offsetX - (Number(pr.offsetX) || 0),
      offsetY: state.offsetY - (Number(pr.offsetY) || 0),
      scaleX: state.scaleX / baseScaleX,
      scaleY: state.scaleY / baseScaleY,
      flipX: !!state.flipX !== !!pr.flipX,
      flipY: !!state.flipY !== !!pr.flipY,
      rotate: state.rotate - (Number(pr.rotation) || 0)
    };
  }
  // Data-space corners [top-left, top-right, bottom-left] of a background image
  // after alignment. Offset, scale and flip are read in the image's own,
  // unrotated coordinates (about its bounds centre); rotation comes last, about
  // the data origin, exactly as R rotates cell coordinates. A preset found on
  // the unrotated slice therefore keeps working once `rotation` is added.
  function imageCorners(bounds, state) {
    var cx = (Number(bounds.xmin) + Number(bounds.xmax)) / 2;
    var cy = (Number(bounds.ymin) + Number(bounds.ymax)) / 2;
    var sx = (Number(state.scaleX) || 1) * (state.flipX ? -1 : 1);
    var sy = (Number(state.scaleY) || 1) * (state.flipY ? -1 : 1);
    var tx = cx + (Number(state.offsetX) || 0);
    var ty = cy + (Number(state.offsetY) || 0);
    var theta = (Number(state.rotate) || 0) * Math.PI / 180;
    var cos = Math.cos(theta), sin = Math.sin(theta);
    function place(x, y) {
      var ax = tx + sx * (Number(x) - cx), ay = ty + sy * (Number(y) - cy);
      return theta ? [ax * cos - ay * sin, ax * sin + ay * cos] : [ax, ay];
    }
    return [place(bounds.xmin, bounds.ymax), place(bounds.xmax, bounds.ymax),
      place(bounds.xmin, bounds.ymin)];
  }
  // Order-independent text for a background identity, so two equal identities
  // compare equal however their object keys were ordered.
  function canonicalIdentity(value) {
    if (value == null) return '';
    if (typeof value !== 'object') return JSON.stringify(value);
    if (Array.isArray(value)) {
      return '[' + value.map(canonicalIdentity).join(',') + ']';
    }
    return '{' + Object.keys(value).sort().map(function (name) {
      return JSON.stringify(name) + ':' + canonicalIdentity(value[name]);
    }).join(',') + '}';
  }
  window.CerebroBackgroundModel = Object.freeze({
    NONE: IMG_NONE,
    canonicalIdentity: canonicalIdentity,
    images: spatialImages,
    currentImage: currentImage,
    stateKey: backgroundStateKey,
    imageKey: imgKey,
    presetState: presetState,
    renderState: imageRenderState,
    corners: imageCorners
  });
})();
