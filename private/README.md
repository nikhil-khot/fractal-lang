# Developer Documentation


## Escape Time Fractals
_Located at /fractals/escapeTime/etfractal.rkt_

Whenever an escape time fractal is generated, it creates a new et-fractal struct.

(render ...) is a simple macro that deconstructs the horizontal and vertical complex bounds and translates the data into
a render state stuct.
The render-state struct is used to save the infromation about the user described fractal to draw on the frame. 








