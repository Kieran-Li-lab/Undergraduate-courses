# Trust-Region Dogleg on Rosenbrock (a=100)

# English comments; exact gradient/Hessian; two policies

# Imports
import numpy as np

# Rosenbrock objective, gradient, Hessian (a=100 by default)
def rosenbrock(x, a=100.0):
    x1, x2 = float(x[0]), float(x[1])
    return a*(x2 - x1**2)**2 + (1.0 - x1)**2

def rosen_grad(x, a=100.0):
    x1, x2 = float(x[0]), float(x[1])
    g1 = -4.0*a*x1*(x2 - x1**2) - 2.0*(1.0 - x1)
    g2 =  2.0*a*(x2 - x1**2)
    return np.array([g1, g2], dtype=float)

def rosen_hess(x, a=100.0):
    x1, x2 = float(x[0]), float(x[1])
    H11 = -4.0*a*(x2 - x1**2) + 8.0*a*x1**2 + 2.0
    H12 = -4.0*a*x1
    H22 =  2.0*a
    return np.array([[H11, H12],[H12, H22]], dtype=float)

# Dogleg step (with tiny PD shift if needed)
def dogleg_step(g, B, Delta, eps_pd=1e-12):
    # Ensure positive definiteness numerically
    w, V = np.linalg.eigh(B)
    if np.min(w) <= eps_pd:
        B = B + (eps_pd - np.min(w) + 1e-8) * np.eye(B.shape[0])

    # Cauchy point on the ray -g
    gBg = float(g @ (B @ g))
    if gBg <= 0:  # safeguard for near-indefinite numerics
        pU = -Delta * g / (np.linalg.norm(g) + 1e-16)
    else:
        alpha_sd = (g @ g) / gBg
        pU = -alpha_sd * g

    # Full Newton step
    pB = -np.linalg.solve(B, g)
    if np.linalg.norm(pB) <= Delta:
        return pB

    # Intersect segment [pU, pB] with sphere ||p||=Delta
    d = pB - pU
    a = float(d @ d)
    b = 2.0 * float(pU @ d)
    c = float(pU @ pU) - Delta**2
    disc = max(0.0, b*b - 4.0*a*c)
    t1 = (-b + np.sqrt(disc)) / (2.0*a) if a > 0 else 0.0
    t2 = (-b - np.sqrt(disc)) / (2.0*a) if a > 0 else 0.0
    cand = [t for t in (t1, t2) if 0.0 <= t <= 1.0]
    t = cand[0] if cand else min(max(t1, 0.0), 1.0)
    return pU + t * d

# Trust-region loop (Dogleg)
def tr_dogleg(x0, a=100.0, Delta0=1.0, Deltamax=100.0,
              rho_lo=0.25, rho_hi=0.75, eta=0.0,
              shrink=0.25, grow=2.0, gtol=1e-8, maxit=200):
    x = np.array(x0, dtype=float).reshape(2)
    Delta = float(Delta0)
    hist = []
    for k in range(maxit):
        f = rosenbrock(x, a=a)
        g = rosen_grad(x, a=a)
        B = rosen_hess(x, a=a)
        gnorm = np.linalg.norm(g)
        hist.append({'k': k, 'x': x.copy(), 'f': f, 'gnorm': gnorm, 'Delta': Delta})
        if gnorm < gtol:
            break

        p = dogleg_step(g, B, Delta)

        # Predicted reduction
        mp = f + g @ p + 0.5 * p @ (B @ p)
        pred = f - mp

        # Actual reduction
        f_new = rosenbrock(x + p, a=a)
        ared = f - f_new

        # Ratio
        rho = ared / (pred + 1e-16)

        # Radius update
        if rho < rho_lo:
            Delta *= shrink
        elif (rho > rho_hi) and (np.linalg.norm(p) >= Delta - 1e-14):
            Delta = min(grow * Delta, Deltamax)

        # Accept/reject
        if rho > eta:
            x = x + p

    return x, hist

if __name__ == "__main__":
    x0 = [-1.2, 1.0]
    xA, H_A = tr_dogleg(x0, a=100.0, Delta0=1.0, Deltamax=100.0,
                        rho_lo=0.25, rho_hi=0.75, eta=0.0,
                        shrink=0.25, grow=2.0, gtol=1e-8, maxit=200)
    xB, H_B = tr_dogleg(x0, a=100.0, Delta0=0.5, Deltamax=50.0,
                        rho_lo=0.20, rho_hi=0.80, eta=1e-3,
                        shrink=0.50, grow=1.50, gtol=1e-8, maxit=200)

    def tail(hist):
        last = hist[-1]
        return last['k'], last['x'], last['f'], last['gnorm'], last['Delta']

    kA, xA_last, fA, gA, DA = tail(H_A)
    kB, xB_last, fB, gB, DB = tail(H_B)

    print("Policy A:", f"iters={kA+1}, x*={xA_last}, f*={fA:.3e}, ||g||={gA:.2e}, Δ={DA:.3f}")
    print("Policy B:", f"iters={kB+1}, x*={xB_last}, f*={fB:.3e}, ||g||={gB:.2e}, Δ={DB:.3f}")