
# Rosenbrock minimization with Armijo backtracking (SD & Newton)
import numpy as np
import pandas as pd

ALPHA_BAR = 1.0
RHO       = 0.5
C_ARMIJO  = 1e-4
TOL       = 1e-8
MAX_SD_IT = 2000
MAX_NT_IT = 200

def rosenbrock(x):
    x1, x2 = x[0], x[1]
    return 100.0*(x2 - x1**2)**2 + (1.0 - x1)**2

def grad_rosenbrock(x):
    x1, x2 = x[0], x[1]
    df_dx1 = -400.0*x1*(x2 - x1**2) - 2.0*(1.0 - x1)
    df_dx2 = 200.0*(x2 - x1**2)
    return np.array([df_dx1, df_dx2])

def hess_rosenbrock(x):
    x1, x2 = x[0], x[1]
    h11 = 1200.0*x1**2 - 400.0*x2 + 2.0
    h12 = -400.0*x1
    h22 = 200.0
    return np.array([[h11, h12],[h12, h22]])

def backtracking(f, grad, xk, pk, alpha_bar=ALPHA_BAR, rho=RHO, c=C_ARMIJO, max_backtracks=60):
    alpha = alpha_bar
    fk = f(xk)
    gk = grad(xk)
    slope0 = np.dot(gk, pk)
    if slope0 >= 0:
        pk = -pk
        slope0 = np.dot(gk, pk)
    bt = 0
    while f(xk + alpha*pk) > fk + c*alpha*slope0 and bt < max_backtracks:
        alpha *= rho
        bt += 1
    return alpha, pk, bt

def steepest_descent(x0, max_iter=MAX_SD_IT, tol=TOL):
    x = np.array(x0, dtype=float)
    records = []
    for k in range(max_iter):
        g = grad_rosenbrock(x)
        gn = np.linalg.norm(g)
        fval = rosenbrock(x)
        if gn <= tol:
            records.append((k, x[0], x[1], fval, gn, np.nan, 0))
            break
        pk = -g
        alpha, pk_adj, bt = backtracking(rosenbrock, grad_rosenbrock, x, pk)
        x = x + alpha*pk_adj
        records.append((k, x[0], x[1], rosenbrock(x), np.linalg.norm(grad_rosenbrock(x)), alpha, bt))
    return pd.DataFrame(records, columns=["iter","x1","x2","f(x)","||grad||","alpha","backtracks"])

def newton_method(x0, max_iter=MAX_NT_IT, tol=TOL):
    x = np.array(x0, dtype=float)
    records = []
    for k in range(max_iter):
        g = grad_rosenbrock(x)
        gn = np.linalg.norm(g)
        fval = rosenbrock(x)
        if gn <= tol:
            records.append((k, x[0], x[1], fval, gn, np.nan, 0))
            break
        H = hess_rosenbrock(x)
        try:
            pk = -np.linalg.solve(H, g)
        except np.linalg.LinAlgError:
            pk, *_ = np.linalg.lstsq(H, -g, rcond=None)
        alpha, pk_adj, bt = backtracking(rosenbrock, grad_rosenbrock, x, pk)
        x = x + alpha*pk_adj
        records.append((k, x[0], x[1], rosenbrock(x), np.linalg.norm(grad_rosenbrock(x)), alpha, bt))
    return pd.DataFrame(records, columns=["iter","x1","x2","f(x)","||grad||","alpha","backtracks"])

if __name__ == "__main__":
    x0_easy = (1.2, 1.2)
    x0_hard = (-1.2, 1.0)
    sd_easy = steepest_descent(x0_easy)
    sd_hard = steepest_descent(x0_hard)
    nt_easy = newton_method(x0_easy)
    nt_hard = newton_method(x0_hard)
    sd_easy.to_csv("SD_x0_1p2_1p2.csv", index=False)
    sd_hard.to_csv("SD_x0_-1p2_1.csv", index=False)
    nt_easy.to_csv("NM_x0_1p2_1p2.csv", index=False)
    nt_hard.to_csv("NM_x0_-1p2_1.csv", index=False)
    print("Saved CSVs.")
