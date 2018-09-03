unit Unit2;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, math, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLType, MisUtils;
const  //Image dimensions
  WIDTH_IMAGE = 640;
  HEIGH_IMAGE = 480;
  FACT_REDUCCION = 8;
  WIDTH_MAP = WIDTH_IMAGE div FACT_REDUCCION;
  HEIGH_MAP = HEIGH_IMAGE div FACT_REDUCCION;
const //Trunk body settings
  TRK_MIN_WIDTH = 3; //Minimal width
  TRK_FACTOR = 1.3;  //Heigh/Width
const  //Set drawing pixel size for MiniMap
  PIXEL_SIZE = 4;
type
  {Extremity join}

  { TJoint }

  TJoint = class
    x,y,z: Single;  //Coordinates
    theta: Single;  //Angle theta
    lenSeg: Single; //Segment length
    procedure ScaleWith(factor: Integer);
  end;
  { TMiniMap }

  {The object MiniMap contain a small map of a Image.
  Include drawing methods using a PaintBox as output.}
  TMiniMap = Class
  private
    pb: TPaintBox;     //PaintBox for output
    cv: TCanvas;       //Canvas for output
    points: array[0..WIDTH_MAP, 0..HEIGH_MAP] of TColor;
    function FrameFilledExists(const frame: Trect): boolean;
    function CanExpandFrameTo(const frame1, frame2: TRect): boolean;
    function HorizontalSegExist(x, y, nPoints: integer): boolean;
    function ScanBigerRectangle(x1, y1, x2, y2: integer; out frameMax: TRect;
      var recWidth: integer): boolean;
  public  //Joints
    Head     : TJoint;
    Neck     : TJoint;
    rShoulder: TJoint;
    lShoulder: TJoint;
    rHip     : TJoint;
    lHip     : TJoint;
  public
    procedure FillMiniMap(Img: TImage);
    procedure Pixel(x, y: Integer; col: TColor);
    procedure Pixel(x, y: Integer);
    procedure Pixel(joint: TJoint; col: TColor);
    procedure Line(x1, y1, x2, y2: Integer; col: TColor);
    procedure Line(joint1, joint2: TJoint; col: TColor);
    procedure Rectangle(x1, y1, x2, y2: Integer; col: TColor);
    procedure DrawInPaintBox;
    procedure ExpandFrame(var frame: Trect; newWidth: integer);
    function ProjectSegment(x1, y1, x2, y2: Integer; draw: boolean): Single;
    function ProjectSegmentR(joint1: TJoint; angTheta: Single; draw: boolean): Single;
    function ProjectSegmentL(joint1: TJoint; angTheta: Single; draw: boolean): Single;
    procedure DetectFirstSegmentR(joint: TJoint; minTheta, maxTheta: Single);
    procedure DetectFirstSegmentL(joint: TJoint; minTheta, maxTheta: Single);
    procedure DetectSkeleton;
    procedure DrawSkeleton;
  public //Initialization
    procedure SetOutputDrawing(pb0: TPaintBox);
    constructor Create;
    destructor Destroy; override;
  end;

  { TForm2 }
  TForm2 = class(TForm)
  published
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1Paint(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    n: integer;
  public
    minimap: TMiniMap;
    procedure ReloadImage;
  end;

var
  Form2: TForm2;

implementation
{$R *.lfm}

{ TJoint }
procedure TJoint.ScaleWith(factor: Integer);
begin
  x := x * factor;
  y := y * factor;
  z := z * factor;
end;
procedure TMiniMap.FillMiniMap(Img: TImage);
{Reduce la imagen pasándola a la matriz Pixels[], para qeu se pueda explorar mejor.
Aquí se elige el método de llenado}
  function GetPixel(X, Y: integer): TColor;
  var
    ScanData: PRGBQuad;
    ValR, ValG, ValB: Byte;
  begin
//    Result := Img.Picture.Bitmap.Canvas.Pixels[X, Y];

    // Point to the Pixel location
    ScanData := Img.Picture.Bitmap.ScanLine[Y];
    Inc(ScanData, X);
    // Get RGB value of the Pixel
    ValR := ScanData^.rgbRed;
    ValG := ScanData^.rgbGreen;
    ValB := ScanData^.rgbBlue;
    // Show information of the Pixel
    Result := RGBToColor(ValR, ValG, ValB);
  end;
var
  y, x, x1, y1: Integer;
begin
  for y:=0 to HEIGH_MAP-1 do begin
    for x:=0 to WIDTH_MAP-1 do begin
      x1 := x * FACT_REDUCCION;
      y1 := y * FACT_REDUCCION;
      if x1>Img.Picture.Width-1 then begin
        MsgErr('Error en tamaño de imagen');
        points[x,y] := clBlack;
        continue;
      end;
      if y1>Img.Picture.Height-1 then begin
        MsgErr('Error en tamaño de imagen');
        points[x,y] := clBlack;
        continue;
      end;
      points[x,y] := GetPixel(x1, y1);
    end;
  end;
end;
procedure TMiniMap.Pixel(x, y: Integer; col: TColor);
{Draw a pixel in the PintBox, using the size defined in PIXEL_SIZE.}
var
  x1, y1: Integer;
begin
  cv.Pen.Color := col;
  cv.Brush.Color := col;
  x1 := x * PIXEL_SIZE;
  y1 := y * PIXEL_SIZE;
  cv.Rectangle(x1, y1, x1+PIXEL_SIZE,y1+PIXEL_SIZE);
end;
procedure TMiniMap.Pixel(x, y: Integer); inline;
{Draw a pixel in the PintBox, using the size defined in PIXEL_SIZE.}
var
  x1, y1: Integer;
begin
  x1 := x * PIXEL_SIZE;
  y1 := y * PIXEL_SIZE;
  cv.Rectangle(x1, y1, x1+PIXEL_SIZE,y1+PIXEL_SIZE);
end;
procedure TMiniMap.Pixel(joint: TJoint; col: TColor);
var
  y1, x1: integer;
begin
  cv.Pen.Color := col;
  cv.Brush.Color := col;
  x1 := Round(joint.x * PIXEL_SIZE);
  y1 := Round(joint.y * PIXEL_SIZE);
  cv.Rectangle(x1, y1, x1+PIXEL_SIZE,y1+PIXEL_SIZE);
end;
procedure TMiniMap.Line(x1, y1, x2, y2: Integer; col: TColor);
var
  dY, dX, k, IncYi, IncXi, IncYr, IncXr, avR, av, avI, X, Y: Integer;
begin
  cv.Pen.Color := col;
  cv.Brush.Color := col;
  //cv.Pen.Width := PIXEL_SIZE;
  //x1 := x1 * PIXEL_SIZE + PIXEL_SIZE div 2;
  //y1 := y1 * PIXEL_SIZE + PIXEL_SIZE div 2;
  //x2 := x2 * PIXEL_SIZE + PIXEL_SIZE div 2;
  //y2 := y2 * PIXEL_SIZE + PIXEL_SIZE div 2;
  //cv.Line(x1, y1, x2, y2);

  //Bresenham algorithm to draw lines
  // 0 - Distancias que se desplazan en cada eje
  dY := (Y2 - Y1);
  dX := (X2 - X1);
  //Incrementos para las secciones con avance inclinado
  if (dY >= 0) then begin
      IncYi := 1;
  end else begin
      dY := -dY;
      IncYi := -1;
  end;
  if (dX >= 0) then begin
      IncXi := 1;
  end else begin
      dX := -dX;
      IncXi := -1;
  end;
  //Incrementos para las secciones con avance recto:
  if (dX >= dY) then begin
      IncYr := 0;
      IncXr := IncXi;
  end else begin
      IncXr := 0;
      IncYr := IncYi;
      // Cuando dy es mayor que dx, se intercambian, para reutilizar el mismo bucle.
      // ver octantes blancos en la imagen encima del código
      k := dX; dX := dY; dY := k;
  end;
  //Inicializar valores (y de error).
  X := X1; Y := Y1;
  avR := (2 * dY);
  av := (avR - dX);
  avI := (av - dX);
  //Bucle para el trazado de las línea.
  repeat
      Pixel(X, Y); // Como mínimo se dibujará siempre 1 píxel (punto).
      if (av >= 0) then begin
          X  := (X + IncXi);     // X aumenta en inclinado.
          Y  := (Y + IncYi);     // Y aumenta en inclinado.
          av := (av + avI);     // Avance Inclinado
      end else begin
          X  := (X + IncXr);     // X aumenta en recto.
          Y  := (Y + IncYr);     // Y aumenta en recto.
          av := (av + avR);     // Avance Recto
      end;
  until (X = X2) and (Y = Y2);
end;
procedure TMiniMap.Line(joint1, joint2: TJoint; col: TColor);
begin
  Line(Round(joint1.x), Round(joint1.y), Round(joint2.x), Round(joint2.y), col );
end;
procedure TMiniMap.Rectangle(x1, y1, x2, y2: Integer; col: TColor);
begin
  cv.Pen.Color := col;
  cv.Pen.Width := PIXEL_SIZE;
  cv.Brush.Color := clNone;
  x1 := x1 * PIXEL_SIZE + PIXEL_SIZE div 2;
  y1 := y1 * PIXEL_SIZE + PIXEL_SIZE div 2;
  x2 := x2 * PIXEL_SIZE + PIXEL_SIZE div 2;
  y2 := y2 * PIXEL_SIZE + PIXEL_SIZE div 2;
  cv.Rectangle(x1, y1, x2, y2);
end;
procedure TMiniMap.DrawInPaintBox;
var
  y, x: Integer;
begin
  for y:=0 to HEIGH_MAP-1 do begin
    for x:=0 to WIDTH_MAP-1 do begin
      Pixel(x, y, points[x, y]);
    end;
  end;
end;
procedure TMiniMap.ExpandFrame(var frame: Trect; newWidth: integer);
begin
  frame.Width := newWidth;
  frame.Height := round(newWidth * TRK_FACTOR);
end;
function TMiniMap.ProjectSegment(x1, y1, x2, y2: Integer; draw: boolean): Single;
{Proyect a segment from the current position in the direction indicated, considering
to pass only for points with data.}
var
  dY, dX, k, IncYi, IncXi, IncYr, IncXr, avR, av, avI, X, Y: Integer;
begin
  //Use the Bresenham algorith to detect points of rect
  dY := (Y2 - Y1);
  dX := (X2 - X1);
  if (dY >= 0) then begin
      IncYi := 1;
  end else begin
      dY := -dY;
      IncYi := -1;
  end;
  if (dX >= 0) then begin
      IncXi := 1;
  end else begin
      dX := -dX;
      IncXi := -1;
  end;
  if (dX >= dY) then begin
      IncYr := 0;
      IncXr := IncXi;
  end else begin
      IncXr := 0;
      IncYr := IncYi;
      k := dX; dX := dY; dY := k;
  end;
  X := X1; Y := Y1;
  avR := (2 * dY);
  av := (avR - dX);
  avI := (av - dX);
  repeat
      if points[X,Y] = 0 then exit(sqrt( (x1-x)**2 + (y1-y)**2 ));
      if draw then Pixel(X, Y); // Como mínimo se dibujará siempre 1 píxel (punto).
      if (av >= 0) then begin
          X  := (X + IncXi);     // X aumenta en inclinado.
          Y  := (Y + IncYi);     // Y aumenta en inclinado.
          av := (av + avI);     // Avance Inclinado
      end else begin
          X  := (X + IncXr);     // X aumenta en recto.
          Y  := (Y + IncYr);     // Y aumenta en recto.
          av := (av + avR);     // Avance Recto
      end;
  until (X = X2) and (Y = Y2);
end;
function TMiniMap.ProjectSegmentR(joint1: TJoint; angTheta: Single; draw: boolean): Single;
{Project a Segment from a Right extremity. Return the segment length.
 The angle is entered in degress.
 The zero angle correspond to the extremity ponting to down:

 --------------> Y
 |\
 |  \
 |    \
 |      \
 |        \
 |  theta-> \
 |           45º
 X(0º)

}
var
  x1, y1: integer;
  x2, y2: integer;
begin
  x1 := Round(joint1.x);
  y1 := Round(joint1.y);
  //Calculate a far second Point
  x2 := Round(x1 + 1000 * sin(angTheta*3.1415926/180));
  y2 := Round(y1 + 1000 * cos(angTheta*3.1415926/180));
  Result := ProjectSegment(x1 , y1, x2, y2, draw);
end;
function TMiniMap.ProjectSegmentL(joint1: TJoint; angTheta: Single; draw: boolean): Single;
{Project a Segment from a Left extremity. Return the segment length.
 The angle is entered in degress.
 The zero angle correspond to the extremity ponting to down:

Y <----------
           /|
         /  |
       /    |
     /      |
   /        |
 /  <-theta |
45º         |
            X(0º)
}
var
  x1, y1: integer;
  x2, y2: integer;
begin
  x1 := Round(joint1.x);
  y1 := Round(joint1.y);
  //Calculate a far second Point
  x2 := Round(x1 - 1000 * sin(angTheta*3.1415926/180));
  y2 := Round(y1 + 1000 * cos(angTheta*3.1415926/180));
  Result := ProjectSegment(x1 , y1, x2, y2, draw);
end;
procedure TMiniMap.DetectFirstSegmentR(joint: TJoint; minTheta, maxTheta: Single
  );
var
  angStart, tmp: Single;
begin
  angStart := minTheta;
  joint.lenSeg := 0;
  joint.theta:= angStart;  //Assumed this angle
  while angStart<maxTheta do begin
    tmp := ProjectSegmentR(joint, angStart, false);
    if tmp > joint.lenSeg then begin
       joint.lenSeg := tmp;
       joint.theta := angStart;
    end;
    angStart := angStart + 7;
  end;
end;
procedure TMiniMap.DetectFirstSegmentL(joint: TJoint; minTheta, maxTheta: Single
  );
var
  angStart, tmp: Single;
begin
  angStart := minTheta;
  joint.lenSeg := 0;
  joint.theta := angStart;  //Assumed this angle
  while angStart<maxTheta do begin
    tmp := ProjectSegmentL(joint, angStart, false);
    if tmp > joint.lenSeg then begin
       joint.lenSeg := tmp;
       joint.theta := angStart;
    end;
    angStart := angStart + 7;
  end;
end;
function TMiniMap.HorizontalSegExist(x, y, nPoints: integer): boolean; inline;
{Indicates if there are consecutive horizontal pixels with values.}
var
  ix: Integer;
begin
  for ix := x to x + nPoints-1 do begin
    if points[ix,y] = 0 then exit(false);  //Empty
  end;
  //All with data
  exit(true);
end;
function TMiniMap.FrameFilledExists(const frame: Trect): boolean;
{Indicates if there are an rectangular área with data in the pixels}
var
  iy: Integer;
begin
  for iy := frame.Top to frame.Bottom do begin
    if not HorizontalSegExist(frame.Left, iy , frame.Width) then exit(false);
  end;
  exit(true);
end;
function TMiniMap.CanExpandFrameTo(const frame1, frame2: TRect): boolean;
{Indicates if the frame frame1 can be expanded to frame2, whtas is teh area coverded
by frame 2 has data.
frame1 and frame2 have the same left-top coordinate and teh xpansion is to right and
down:
+--------+----+
|        |    |
| frame1 |    |
|        |    |
+--------+    |
     frame2   |
--------------+
}
var
  y, x: LongInt;
begin
  //Test right area
  for y:=frame1.Top to frame1.Bottom do begin
    for x:=frame1.Right+1 to frame2.Right do begin
      if points[x,y] = 0 then exit(false);
    end;
  end;
  //Test down area
  for y:=frame1.Bottom+1 to frame2.Bottom do begin
    for x:=frame2.left to frame2.Right do begin
      if points[x,y] = 0 then exit(false);
    end;
  end;
  exit(true);
end;
function TMiniMap.ScanBigerRectangle(x1,y1,x2,y2: integer;
                                      out frameMax: TRect;
                                      var recWidth: integer): boolean;
{Scan for the biger rectangle can be located from some point inside the área
(x1,y1,x2,y2).
If doesn't find a rectangle, returns FALSE.}
var
  y, x: Integer;
  frame: TRect;
begin
  frameMax.width := 0;
  y := 0;
  while y <  y2 do begin
    x := x1;
    while x<x2 do begin
      //Find at least 3 pixels width
      frame.Left := x;
      frame.Top  := y;
      ExpandFrame(frame, recWidth);  //Set frame size
      if FrameFilledExists(frame) then begin
        //Pixel(x,y, clRed);
        repeat
          inc(recWidth);   //Nuevo tamaño
          ExpandFrame(frame, recWidth);  //prueba ampliando
        until not FrameFilledExists(frame);
        dec(recWidth);   //retorna
        //This is th biggest rectangle size can be located here
        if frame.Width> frameMax.Width then begin
          //Later rectangles arer used only if they are bigger.
          frameMax := frame;
        end;
      end;
      inc(x);
    end;
    inc(y);
  end;
  Result := frameMax.Width > 0;
end;
procedure TMiniMap.DetectSkeleton;
{Detect the body trunk and positions of joints.
Only works on vertticals body and preference with spaced legs.
Works only analyzing a 2D surface.}
const
  INI_TRUNK_WIDTH = 5;
var
  xLeft, xRight: Integer;
  yBottom, recWidth: Integer;
  frame: TRect;
begin
  //The body trunk s expected to be found in the central area of the screen
  //That's why we don't scan all for all the width.
  xLeft := WIDTH_MAP div 3 - 1;
  xRight := 2*(WIDTH_MAP div 3);
  yBottom := round(0.8 * HEIGH_MAP);  //Not expect the trunk to low
  recWidth := INI_TRUNK_WIDTH;
  if not ScanBigerRectangle(xLeft, 0, xRight, yBottom, frame, recWidth) then exit;
  frame.Right := frame.Right - 2;  //Corrección
//  Rectangle(frame.Left, frame.Top, frame.Right, frame.Bottom, clRed);
//Line(frame.Left-1, frame.Top, frame.Right-1, frame.Top, clRed);
//Line(frame.Left-1, frame.Bottom, frame.Right-1, frame.Bottom, clRed);
//Line(frame.Left-1+frame.Width div 2, frame.Top,
//     frame.Left-1+frame.Width div 2, frame.Bottom, clRed);

  //Locate Joints in Body
  Neck.x := frame.Left + frame.Width div 2 ;
  Neck.y := frame.Top - 0.1 * frame.Height;

  rShoulder.x := frame.Right;
  rShoulder.y := frame.Top + 0.1 * frame.Height;
  rShoulder.z := 0;

  lShoulder.x := frame.Left;
  lShoulder.y := frame.Top + 0.1 * frame.Height;
  lShoulder.z := 0;

  rHip.x := frame.Right - 0.2 * frame.Width;
  rHip.y := frame.Bottom;
  rHip.z := 0;

  lHip.x := frame.Left + 0.2 * frame.Width;;
  lHip.y := frame.Bottom;
  lHip.z := 0;

  //Detects neck angle.
  DetectFirstSegmentR(rShoulder, 3, 180);
  Head.x := frame.Left + frame.Width div 2 ;
  Head.y := frame.Top - 0.3 * frame.Height;

  //Detects arms direction. Start in 3º, almost fall arms.
  DetectFirstSegmentL(lShoulder, 3, 180);
  //Detects legs direction
  DetectFirstSegmentR(rHip,0, 90);
  DetectFirstSegmentL(lHip,0, 90);
  //Detect Neck direction
  DetectFirstSegmentR(Neck, 90, 270);

end;
procedure TMiniMap.DrawSkeleton;
begin
  //Draw Head and Neck
//  Line(Head, Neck, clRed);
  //Segment from Neck to trunk
  Line(Round(Neck.x), Round(Neck.y), Round(Neck.x), Round(lShoulder.y), clRed);
  //Segment from Neck to head
  //ProjectSegmentR(Neck, Neck.theta, true);

  //Draw trunk
  Line(lShoulder, rShoulder, clRed);
  Line(lHip, rHip, clRed);
  Line(lShoulder, rHip, clRed);
  Line(rShoulder, lHip, clRed);

  //Draw segments found
  cv.Pen.Color := clRed;
  cv.Brush.Color := clRed;
  ProjectSegmentL(lShoulder, lShoulder.theta, true);
  ProjectSegmentR(rShoulder, rShoulder.theta, true);
  ProjectSegmentL(lHip, lHip.theta, true);
  ProjectSegmentR(rHip, rHip.theta, true);

  //Draw joints
  Pixel(Head, clBlue);
  Pixel(Neck, clBlue);
  Pixel(lHip, clBlue);
  Pixel(rHip, clBlue);
  Pixel(rShoulder, clBlue);
  Pixel(lShoulder, clBlue);
end;
//Initialization
procedure TMiniMap.SetOutputDrawing(pb0: TPaintBox);
begin
  pb := pb0;
  cv := pb.Canvas;
end;
constructor TMiniMap.Create;
begin
  Head     := TJoint.Create;
  Neck     := TJoint.Create;
  rShoulder:= TJoint.Create;
  lShoulder:= TJoint.Create;
  rHip     := TJoint.Create;
  lHip     := TJoint.Create;
end;
destructor TMiniMap.Destroy;
begin
  Head.Destroy;
  Neck.Destroy;
  rShoulder.Destroy;
  lShoulder.Destroy;
  rHip.Destroy;
  lHip.Destroy;
  inherited Destroy;
end;
{ TForm2 }
procedure TForm2.FormCreate(Sender: TObject);
begin
  minimap := TMiniMap.Create;
  minimap.SetOutputDrawing(PaintBox1);
end;
procedure TForm2.FormDestroy(Sender: TObject);
begin
  minimap.Destroy;
end;
procedure TForm2.FormShow(Sender: TObject);
begin
  n := 1;
  ReloadImage;
end;

procedure TForm2.Image1Click(Sender: TObject);
var
  cv: TCanvas;
begin
end;

procedure TForm2.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
//  Shape1.Brush.Color := GetPixel(X, Y);
//  Label1.Caption := 'x'+IntToStr(X)+':y'+IntToStr(Y);
end;

procedure TForm2.Image1Paint(Sender: TObject);
var
  cv: TCanvas;
  procedure Circle(pto: TJoint);
  begin
    cv.Ellipse(Round(pto.x*FACT_REDUCCION-4),
               Round(pto.y*FACT_REDUCCION-4),
               Round(pto.x*FACT_REDUCCION+4),
               Round(pto.y*FACT_REDUCCION+4));
  end;
  procedure Line(pto1: TJoint);
  begin
    cv.Ellipse(Round(pto1.x*FACT_REDUCCION-4),
               Round(pto1.y*FACT_REDUCCION-4),
               Round(pto1.x*FACT_REDUCCION+4),
               Round(pto1.y*FACT_REDUCCION+4));
  end;
begin
  cv := Image1.Canvas;
  cv.Pen.Color := clBlue;
  cv.Pen.Width := 3;
  cv.Brush.Color := clBlue;

  Circle(minimap.Head);
  Circle(minimap.Neck);
  Circle(minimap.lShoulder);
  Circle(minimap.lHip);
  Circle(minimap.rShoulder);
  Circle(minimap.rHip);
//  cv.Line(0, 0,100,100);
end;

procedure TForm2.PaintBox1Paint(Sender: TObject);
begin
  minimap.DrawInPaintBox;
  minimap.DrawSkeleton;
end;
procedure TForm2.ReloadImage;
begin
  Image1.Picture.LoadFromFile('CapturasKinect/'+ IntToStr(n) +'.png');
  //Explora en matriz reducida
  minimap.FillMiniMap(Image1);
Image1.Picture.LoadFromFile('CapturasKinect/'+ IntToStr(n) +'.png');
  minimap.DetectSkeleton;
  PaintBox1.Invalidate;  //Draw MiniMap

end;
procedure TForm2.Button1Click(Sender: TObject);
begin
  if n < 4 then inc(n) else n:=1;
  ReloadImage;
end;
procedure TForm2.Button2Click(Sender: TObject);
begin
  if n>1 then dec(n) else n := 4;
  ReloadImage;
end;

end.

