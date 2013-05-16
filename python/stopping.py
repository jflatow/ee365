"""
Stopping problem from: http://www.stanford.edu/class/ee365/lectures/inf_horiz.pdf
"""
import mdp

def approx(o, o_, tol=1e-3):
    for x, (v, u) in o.items():
        v_, u_ = o_.get(x, (0, None))
        if abs(v - v_) > tol or u != u_:
            return False
    return True

def uniform(X):
    return dict((x, 1.0 / len(X)) for x in X)

class Stopper(mdp.MDP):
    def __init__(self, N=20, gstop={(5, 5): -120, (17, 10): -70, (10, 15): -150}):
        def neighbors(i, j):
            for c, d in [(i, j - 1), (i, j + 1), (i, j), (i - 1, j), (i + 1, j)]:
                if 0 <= c < N and 0 <= d < N:
                    yield c, d
        states = [(i, j) for i in range(N) for j in range(N)]
        self.X = lambda t: states
        self.U = lambda t, x: ('stop', 'hold') if x else ()
        self.W = lambda t, x, u: uniform(list(neighbors(*x))) if u == 'hold' else {None: 1}
        self.step = lambda t, x, u, x_: x_
        self.cost = lambda t, x, u, x_: 1 if u == 'hold' else gstop.get(x, 0)

    def policy(self, V=mdp.Vtot(), o_={}):
        for i, o in self.values(V=V):
            if approx(o, o_):
                return
            yield i, o
            o_ = o

def plot(policy, T=0, alpha=1):
    import mpl_toolkits.mplot3d
    import pylab
    f = pylab.figure()
    o = policy[T]
    a = f.add_subplot(111, projection='3d')
    a.plot_trisurf([i for i, j in o], [j for i, j in o], [v for v, u in o.values()], cmap=pylab.cm.jet)
    a.scatter([i for i, j in o], [j for i, j in o], [100 * (u == 'stop') for v, u in o.values()], alpha=alpha)
    a.set_title('T = %s' % T)
    a.set_xlabel('X1')
    a.set_ylabel('X2')
    a.set_zlabel('Cost / Policy')
    pylab.show()

if __name__ == '__main__':
    for V in (mdp.Vtot(), mdp.Vdsc(.5), mdp.Vdsc(.1), mdp.Vavg((0, 0))):
        policy = dict(Stopper().policy(V=V))
        plot(policy, len(policy) - 1)
