# Developer Documentation


## Escape Time Fractals
_Located at /fractals/escapeTime/etfractal.rkt_

Whenever an escape time fractal is generated, it creates a new et-fractal struct which stores the updater function for the particular 
fractal. By convention in the code, you will see the arguments of the function reffered to as c and z. c refers to the point on the complex
plane which is being observed and z is the accumulated value of running the updater function starting at 0+0i.

(render ...) is a simple macro that deconstructs the horizontal and vertical complex bounds and translates the data into
a render state stuct.

The render-state struct is used to save the infromation about the user described fractal to draw on the frame. 

Rendering the frame occurs in two steps, the first tracks the progress of the computations occuring to show to the user and the second
is showing the graphical representation of the fractal. We utilize threads to be able to observe the operations that have occured so far and
change the display accordingly in the first step.

## Iterative Fractals
_Located at /fractals/iterative/iterative.rkt_

Iterative fractals are represented by `ifractal` structs. IFractals contain a hash table of ids to commands (representing the bindings), a list of ids (representing the current state in order), and a hash table of ids to lists of ids (representing transformations). 

`(generate-ifractal ...)` is a macro that parses the bindings, initial state, and transformations from syntax into the ifractal struct. It first checks to ensure that all ids used in the initial state and transformation are bound, and that no ids were bound or transformed multiple times. It then individually parses the bindings, initial state, and transformations into the forms required for the ifractal struct.

Commands are parsed into functions that take in and return a `turtle-window `struct. TurtleWindows contain a turtle and a stack of saved turtles. Parsed commands deconstruct the provided TurtleWindow, update the turtle and stack accordingly, and reconstruct the TurtleWindow for future commands.

Iterative fractals can be rendered using either `render` or `render/interactive`. `render` executes the commands bound to the ids in the fractal's state and displays the final image as a Pict scaled to the specified size. `render/interactive` opens a window of the specified size and allows the users to control the iteration of the fractal. The window is displayed using big-bang, and the information is kept track of in a world-state struct. The world-state contains the fractal, level of iteration, previously computed images, and window size in order to avoid recalculating fractal states and quickly display images. 