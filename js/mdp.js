(function () {
  var policy = function (mdp, cb) {
    var opt = {}, cb = cb || function () {};
    for (var t = mdp.T; t >= 0; t--)
      cb(t, opt = bellman(t, mdp, opt));
    return opt;
  };

  var bellman = function (t, mdp, opt) {
    var f = mdp.f, g = mdp.g;
    var V = function (x) { return x in opt ? opt[x][0] : 0 };
    return mdp.X(function (o, x) {
        o[x] = mdp.U(function (best, u) {
            var E = mdp.W(function(E, p, w) {
                return E + (g(t, x, u, w) + V(f(t, x, u, w))) * p;
              }, 0, [t, x, u]);
            if (E < best[0])
              return [E, u];
            return best;
          }, [Infinity, undefined], [t, x]);
        return o;
      }, {}, t);
  }

  MDP = {
    policy: policy,
    bellman: bellman,
    fold: function (f, a, o) {
      for (var k in o)
        a = f(a, o[k], k, o);
      return a;
    }
  };
})();