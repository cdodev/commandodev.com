state = { noiseZ: 0, hueCounter: 0, particles: null, field: null  };

exports.setStateImpl = function (st) {
    state = st;
};

exports.getStateImpl = function () {
    return state;
};

exports.frameCountImpl = function(p5) {
    return p5.frameCount;
};
