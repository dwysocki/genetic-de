import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import os.path as path

def plot(fn, filename, output,
         *,
         xmin=0, xmax=1, xstep=0.1, ymin=0, ymax=1, ystep=0.1):
    data = np.loadtxt(filename)

    x  = data[0]
    ys = data[1:]

    mesh = np.meshgrid(np.arange(xmin, xmax, xstep),
                       np.arange(ymin, ymax, ystep))
    xx, yy = mesh
    slope = fn(xx, yy)
    angles = np.arctan(slope)
    x_comps = np.cos(angles)
    y_comps = np.sin(angles)

    for i, y in enumerate(ys):
        plot_vector_field(fn, xx, yy, x_comps, y_comps,
                          xmin,xmax,ymin,ymax)

        # plot y
        plt.scatter(x, y, c='r', marker='o')

        plt.title("Generation {}".format(i))
        plt.xlabel("x")
        plt.ylabel("y")
        plt.savefig(path.join(output, str(i)))
        plt.close('all')

def plot_vector_field(fn, X, Y, X_COMP, Y_COMP,
                      xmin,xmax,ymin,ymax):
    plt.figure()
    Q = plt.quiver(X, Y, X_COMP, Y_COMP)
    plt.axis([xmin,xmax,ymin,ymax])
