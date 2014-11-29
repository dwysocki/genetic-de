import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import os.path as path

def plot(dy_dx, filename, output,
         *,
         exact_solution=None,
         xmin=0, xmax=1, xstep=0.1, ymin=0, ymax=1, ystep=0.1):
    data = np.loadtxt(filename)

    x  = data[0]
    ys = data[1:]

    mesh = np.meshgrid(np.arange(xmin, xmax, xstep),
                       np.arange(ymin, ymax, ystep))
    xx, yy = mesh
    slope = dy_dx(xx, yy)
    angles = np.arctan(slope)
    x_comps = np.cos(angles)
    y_comps = np.sin(angles)

    smooth_x = np.linspace(xmin, xmax, 100)

    for i, y in enumerate(ys):
        plot_vector_field(xx, yy, x_comps, y_comps,
                          xmin,xmax,ymin,ymax)

        # plot y
        plt.scatter(x, y, c='g', marker='o')

        if exact_solution is not None:
            plt.plot(smooth_x, exact_solution(smooth_x), 'r--')

        plt.title("Generation {}".format(i))
        plt.xlabel("x")
        plt.ylabel("y")
        plt.savefig("{}{}".format(output,i))
        plt.close('all')

def plot_vector_field(X, Y, X_COMP, Y_COMP,
                      xmin,xmax,ymin,ymax):
    plt.figure()
    plt.quiver(X, Y, X_COMP, Y_COMP)
    plt.axis([xmin,xmax,ymin,ymax])
