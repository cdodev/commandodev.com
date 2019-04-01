state = { noiseZ: 0, hueCounter: 0, particles: null, field: null  };

exports.calculateFieldImpl = function(dimensions, f) {
    for(var x = 0; x < dimensions.cols; x++) {
        for(var y = 0; y < dimensions.rows; y++) {
            f(x, y, state.field[x][y]);
        }
    }
}

exports.drawParticlesImpl = function(size, dimensions, f) {
    state.particles.forEach(function(p) {
        var x = p.pos.x / size;
        var y = p.pos.y / size;
        var v;
        if(x >= 0 && x < dimensions.cols && y >= 0 && y < dimensions.rows) {
            v = state.field[Math.floor(x)][Math.floor(y)];
        }
        f(p, v);
        // p.move(v);
        // p.wrap();
        // p.draw();
    });
}

exports.initParticlesImpl = function (particles) { state.particles = particles; };

exports.initFieldImpl = function (dimensions) {
    field = new Array(dimensions.cols);
    for(var x = 0; x < dimensions.cols; x++) {
        field[x] = new Array(dimensions.cols);
        for(var y = 0; y < dimensions.rows; y++) {
            field[x][y] = {x: 0, y: 0};
        }
    }
    state.field = field;
}

exports.incrNoiseImpl = function(n) { state.noiseZ += n; };
exports.incrHueImpl = function(n) { state.hueCounter += n; };

exports.setStateImpl = function (st) {
    state = st;
};

exports.getStateImpl = function () {
    return state;
};

function mkImpl(f) {
    return function() {
        var args = arguments;
        return function () {
            return f.apply(null, args);
        };
    };
}

function addTo(vec, v) {
    vec.x += v.x;
    vec.y += v.y;
}

exports.addToImpl = mkImpl(addTo);

function getLength(vec) {
    return Math.sqrt(vec.x * vec.x + vec.y * vec.y);
}

exports.getLengthImpl = mkImpl(getLength);

function setLength(vec, length) {
    var angle = getAngle(vec);
    vec.x = Math.cos(angle) * length;
    vec.y = Math.sin(angle) * length;
}

exports.setLengthImpl = mkImpl(setLength);

function getAngle(vec) {
    return Math.atan2(vec.y, vec.x);
}

exports.getAngleImpl = mkImpl(getAngle);

function setAngle(vec, angle) {
    var length = getLength(vec);
    vec.x = Math.cos(angle) * length;
    vec.y = Math.sin(angle) * length;
}

exports.setAngleImpl = mkImpl(setAngle);

exports.moveImpl = function(particle, acc, maxSpeed) {
    return function () {
        particle.prevPos.x = particle.pos.x;
        particle.prevPos.y = particle.pos.y;
        if(acc) {
            addTo(particle.acc, acc);
        }
        addTo(particle.vel, particle.acc);
        addTo(particle.pos, particle.vel);

        if(getLength(particle.vel) > maxSpeed) {
            setLength(particle.vel, maxSpeed);
        }
        particle.acc.x = 0;
        particle.acc.y = 0;
    };
};

exports.wrapImpl = function(particle, canvas) {
    return function () {
        var w = canvas.width;
        var h = canvas.height;
        if(particle.pos.x > w) {
            particle.prevPos.x = particle.pos.x = 0;
        } else if(particle.pos.x < 0) {
            particle.prevPos.x = particle.pos.x = w - 1;
        }
        if(particle.pos.y > h) {
            particle.prevPos.y = particle.pos.y = 0;
        } else if(particle.pos.y < 0) {
            particle.prevPos.y = particle.pos.y = h - 1;
        }
    };
}

exports.drawBackgroundImpl = function(ctx, canvas) {
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
};
