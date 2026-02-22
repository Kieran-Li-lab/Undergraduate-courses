function ydot=f(t, y)
    ydot=y^2-y*sin(t)+cos(t)
endfunction
y0=0;
t0=0;
t=0:0.1:10;
y = ode(y0,t0,t,f);
comet(t,y)
