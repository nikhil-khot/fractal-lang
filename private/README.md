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






