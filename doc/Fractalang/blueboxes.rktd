33
((3) 0 () 0 () () (h ! (equal)))
procedure
(generate-etfractal func) -> ETFractal
  func : (-> Complex Complex Complex)
procedure
(point-in-set? etf               
               point             
               max-iter          
               escape-bound) -> Boolean
  etf : ETFractal
  point : Complex
  max-iter : Natural
  escape-bound : Natural
procedure
(steps-to-escape etf               
                 point             
                 max-iter          
                 escape-bound) -> (Maybe Natural)
  etf : ETFractal
  point : Complex
  max-iter : Natural
  escape-bound : Natural
procedure
(render etf                               
        #:color-func color-function       
        #:escape-bounds escape-bounds     
        #:horizontal-bounds h-bounds      
        #:vertical-bounds v-bounds        
        #:window-height height            
        #:window-width width)         -> Frame%
  etf : ETFractal
  color-function : (-> Natural Color)
  escape-bounds : Natural
  h-bounds : Bounds
  v-bounds : Bounds
  height : PosInt
  width : PosInt
procedure
(generate-ifractal l-system) -> IFractal
  l-system : L-System
syntax
(draw distance color)
syntax
(move distance)
syntax
(turn degrees)
syntax
(save)
syntax
(return)
syntax
(none)
syntax
(combine command ...+)
procedure
(iterate ifr iterations) -> IFractal
  ifr : IFractal
  iterations : Natural
procedure
(render ifr iterations width height) -> Pict
  ifr : IFractal
  iterations : Natural
  width : PosInt
  height : PosInt
procedure
(render/interactive ifr width height) -> IFractal
  ifr : IFractal
  width : PosInt
  height : PosInt
