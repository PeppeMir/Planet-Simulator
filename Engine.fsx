open System.Windows.Forms
open System.Drawing

type ScrollButton() =

 let mutable principal = new RectangleF()
 let mutable (subrect:RectangleF List) = []
 let mutable pressedUDLR = (false, false, false, false)
 let mutable color = Color.DarkCyan

 member x.getList() = subrect
 
 member x.setColor(c:Color, p:PointF) =
    color <- c
    let a = List.toArray subrect
    if a.[0].Contains(p) then       //up
      pressedUDLR <- (true,false,false,false)
        elif a.[1].Contains(p) then     //right
            pressedUDLR <- (false,false,false,true)
                elif a.[2].Contains(p) then     //down
                    pressedUDLR <- (false,true,false,false)
                        elif a.[3].Contains(p) then     //left
                             pressedUDLR <- (false,false,true,false)

 member x.IsIn(p:PointF) =
   let t = if principal.Contains(p) then true
                                    else false
   t

 member x.PaintBtn(g:Graphics, xx:float32, yy:float32) =

   principal.X <- xx
   principal.Y <- yy
   principal.Width <- 90.f
   principal.Height <- 90.f


   let rUp = new RectangleF(xx + 30.f, yy , 30.f, 30.f)
   let rDw = new RectangleF(xx + 30.f, yy + 60.f, 30.f, 30.f)
   let rSx = new RectangleF(xx, yy + 30.f, 30.f, 30.f)
   let rDx = new RectangleF(xx + 60.f, yy + 30.f, 30.f, 30.f)

   subrect <- rUp :: rDx :: rDw :: rSx :: subrect       // "NOSE" dalla testa 
   
   use pen2 = new Pen(Color.White, 2.f)
   for i in 0 .. 3 do
        let el = subrect.[i]
        g.DrawRectangle(pen2, el.X, el.Y, el.Width, el.Height)
        g.FillRectangle(Brushes.DarkCyan, el)

   use b = new SolidBrush(color)
   match pressedUDLR with
   | (true, false, false, false) -> g.FillRectangle(b, rUp)
   | (false, true, false, false) -> g.FillRectangle(b, rDw)
   | (false, false, true, false) -> g.FillRectangle(b, rSx)
   | (false, false, false, true) -> g.FillRectangle(b, rDx)
   | (_, _, _, _) -> pressedUDLR <- (false,false,false,false)
 
 
 //per lo zoom e l'avvio dell'animazione   
type SpaceButton() =            
 let w = 30.f
 let h = 30.f
 let mutable rect = new RectangleF()
 let mutable pressed = false
 let Colors = [| Brushes.Red; Brushes.Green |]
 let mutable currentIndex = 0

 member x.getRectangle() = rect

 member x.setPressed(v:bool) = 
    pressed <- v
    if pressed then
        currentIndex <- 1
        else
        currentIndex <- 0

 member x.PaintButton(g:Graphics, xx:float32, yy:float32, c:char) =
  
  rect <- new RectangleF(xx, yy, w, h)
  use pen = new Pen(Color.Green, 4.f)
  use pen2 = new Pen(Color.Red, 4.f)

  if c = '>' then
      g.FillRectangle(Colors.[currentIndex], rect)
  
  if c = '-' then
     g.DrawLine(pen2, rect.X, rect.Y + (rect.Height / 2.f), rect.X + rect.Width, rect.Y + (rect.Height / 2.f))

  if c = '+' then
    g.DrawLine(pen, rect.X + (rect.Width / 2.f), rect.Y, rect.X + (rect.Width / 2.f), rect.Y + rect.Height)  
    g.DrawLine(pen, rect.X, rect.Y + (rect.Height / 2.f), rect.X + rect.Width, rect.Y + (rect.Height / 2.f))

 member x.IsIn(p:PointF) =
  let check = if rect.Contains(p) then true
                                  else false
  check

type MassCenter() =
 
 let mutable rect = new RectangleF()
 let mutable center = new PointF()
 let mutable img = null
 let mutable ray = 25.f                   //default
 let G = 6.67458f
 let mutable force = 0.f


 member x.getCenter() = center
 member x.getRectangle() = rect
 member x.getRay() = ray
 member x.getImage() = img
 member x.getForza() = force

 member x.setCenter(p:PointF) =
   center <- new PointF(p.X, p.Y)

 member x.setImage(image:Image) = 
   img <- image

 member x.changeRay(value:float32) =
   ray <- value

 member x.PaintCircle(g:Graphics, xx:float32, yy:float32) =          
  force <- (G * (2.f * ray)) / (ray*ray)
  center <- new PointF(xx,yy)
  use pen = new Pen(Color.Black,2.f)
  if img = null then
      pen.Color <- Color.Red

  use pen2 = new Pen(Color.Gray) 
  
  g.TranslateTransform(xx,yy)             //assi nel centro del cerchio
  
  let mutable increase = 150.f
  let mutable varcolor = Color.Azure.ToArgb
  let mutable rr,gg,bb = 135, 206, 250
  let mutable color = Color.SkyBlue

  while ( increase > 0.f ) do

    let r = new RectangleF( - ray - (increase / 3.f / force) ,  -ray - (increase / 3.f / force), 2.f * (ray + (increase / 3.f / force)), 2.f * (ray + (increase / 3.f / force))  )
    color <- Color.FromArgb(30, rr ,gg, bb)
    use b = new SolidBrush(color)
    g.FillEllipse(b, r)
    rr <- rr - 10
    gg <- gg - 80
    bb <- bb - 90
    increase <- increase - 50.f

  g.TranslateTransform(-xx,-yy)         //assi a posto

  use brush = new SolidBrush(Color.Black)
  rect <- new RectangleF(xx - ray, yy - ray, 2.f * ray, 2.f * ray)       // calcolo rettangolo a partire dal clickpoint, raggio ray
  g.DrawEllipse(pen, rect)                                          //disegno il cerchio come da rettangolo
  g.FillEllipse(brush,rect)

  if img <> null then                                       //se è stata assegnata un immagine, riempio il cerchio con questa
        use brush = new Bitmap(img, int(rect.Width), int(rect.Height))
        use textbrush = new TextureBrush(brush)
        textbrush.TranslateTransform(rect.X, rect.Y)
        g.FillEllipse(textbrush,rect)

////////////////////////////////////////////////////////
////////////////////////////////////////////////////////
////////////////////////////////////////////////////////
   
type Space() as this =
 inherit UserControl()

 let mutable resize = false                     //per il ridimensionamento di un centro di massa
 let mutable provvMassCenter = new MassCenter() //per mantenere il centro di massa da ridmensionare (vedi OnMouseDown)
 let mutable clickPoint = new PointF()
 let mutable zoomIncrButton = new SpaceButton()
 let mutable zoomScaleButton = new SpaceButton()
 let mutable animButton = new SpaceButton()
 let mutable scrollBtn = new ScrollButton()
 let mutable (massList:MassCenter List) = []
 let mutable zoomvalue = 1.f
 let stdRay = 25.f
 let mutable traslationX = 0.f
 let mutable traslationY = 0.f
 let timerAnimation = new Timer(Interval=30)
 let timerScroll = new Timer(Interval=30)

 /////////////////////////////////////////////////  FUNCTION /////////////////////////////////////////////

 let check_postition(p:PointF) =                 //controlla se il punto p appartiene ad uno dei rettangoli(modificati per il campo di forza)                                               
  let mutable trovato = false                   //per evitare sovrapposizioni nel disegno dei centri di massa
  if massList <> [] then
    //check1 : click per disegnare su una posizione già occupata da un altro rettangolo
    //check2: non ci deve essere intersezione tra il rettangolo da disegnare e ogni rettangolo già esistente
    let a = List.toArray massList
    let mutable i = 0
    while ( i < a.Length && not(trovato) ) do               
            let el = a.[i].getRectangle()
            let rwc = new RectangleF(p.X - stdRay, p.Y - stdRay, 2.f * stdRay, 2.f * stdRay)         //ipotetico rettangolo da disegnare
            
             //check1 + check2
            if el.Contains(p) || el.IntersectsWith(rwc) then        
                    trovato <- true           

            i <- i + 1                             
  trovato
  ////end fun

 let search_massCenter() =
    let a = List.toArray massList
    let mutable trovato = false
    let mutable i = 0
    while( not(trovato) && i < a.Length ) do            //SOLO SE "trovato" = true, in "provvMassCenter" sarà contenuto il centro 
      provvMassCenter <- a.[i]                            //di massa al quale appartiene il punto
      let r = provvMassCenter.getRectangle()
      if r.Contains(clickPoint) then
                 trovato <- true
      i <- i+1
    trovato
 
 /////////////////////////////////

 let scrolling(p:PointF) =
    let a = List.toArray (scrollBtn.getList())
    let mutable ok = true
    
    if a.[0].Contains(p) then
     traslationY <- traslationY - float32(4.0)
        elif a.[1].Contains(p) then
            traslationX <- traslationX + float32(4.0)
                elif a.[2].Contains(p) then
                    traslationY <- traslationY + float32(4.0)
                        elif a.[3].Contains(p) then
                             traslationX <- traslationX - float32(4.0)   
                                else
                                 ok <- false
    ok

 ///////////////////////////////////////////////// END FUNCTION /////////////////////////////////////////////

 do 
   timerAnimation.Tick.Add( fun _ -> this.Invalidate() )
   timerScroll.Tick.Add( fun _ ->  
        let scrolled = scrolling(clickPoint)
        if scrolled then
            this.Invalidate() )

   this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
   this.SetStyle(ControlStyles.AllPaintingInWmPaint, true)
   //this.BackgroundImage <- Image.FromFile( @"C:\Users\Giuseppe\Desktop\costellazioni.jpg") //sfondo costellazioni 
   this.BackColor <- Color.Black

 member x.Animate(a:MassCenter[]) =

    for i in 0 .. a.Length - 1 do
     if a.Length > 1 then
      for j in i+1 .. a.Length - 1 do
           
           let x1 = a.[i].getCenter().X
           let x2 = a.[j].getCenter().X
           let y1 = a.[i].getCenter().Y
           let y2 = a.[j].getCenter().Y

           let quad1 = (x1 - x2) * (x1 - x2)        //distanza tra i due centri di massa(Pitagora)
           let quad2 = (y1 - y2) * (y1 - y2)
           let dist = sqrt(quad1 + quad2)               

           let r1 = a.[i].getRay()
           let r2 = a.[j].getRay()

           if dist > (r1+r2) then        // check "gnam" :)         
                
                let forza = ((0.667458f) * single(2.f * r1) * single(2.f * r2)) / (dist*dist)     // F = (G * m1 * m2) / r^2
                
                //modulo dell'accelerazione, rispettivamente per i due centri di massa: 2° principio dinamica (Newton): F = ma
                let mutable acc1 = ( (-forza) / single(a.[i].getRay())) * 100.f
                let mutable acc2 = (forza / single(a.[j].getRay()))  * 100.f

                let acc1x = acc1 * ((x1 - x2) / dist)                             // (modulo accelerazione vettoriale) |a| * (catetox / ipotenusa) 
                let acc1y = acc1 * ((y1 - y2) / dist)                             // |a| * (catetoy / ipotenusa)
                let acc2x = acc2 * ((x1 - x2) / dist)
                let acc2y = acc2 * ((y1 - y2) / dist)
                
                a.[i].setCenter( new PointF (single(x1 + acc1x), single(y1 + acc1y)) )      //aggiornamento posizioni in base all'accelerazione
                a.[j].setCenter( new PointF(single(x2 + acc2x), single(y2 + acc2y)) )            

 member x.ZoomIn() =   
   zoomvalue <- zoomvalue * float32(1.2)
   x.Invalidate()

 member x.ZoomOut() =  
   zoomvalue <- zoomvalue / float32(1.2)
   x.Invalidate()

 override x.OnKeyDown e =
  base.OnKeyDown e

  let mutable ok = true

  match e.KeyData with
  | Keys.W -> traslationY <- traslationY - float32(4.0)
  | Keys.A -> traslationX <- traslationX - float32(4.0)
  | Keys.D -> traslationX <- traslationX + float32(4.0)
  | Keys.S -> traslationY <- traslationY + float32(4.0)
  | Keys.Enter -> timerAnimation.Start()
                  animButton.setPressed(true)
  | Keys.Space -> timerAnimation.Stop()
                  animButton.setPressed(false)
  | _ -> ok <- false

  if ok then
    x.Invalidate()

 override x.OnDoubleClick e =
  base.OnDoubleClick e
  let trovato = search_massCenter()
  if not(zoomIncrButton.IsIn(clickPoint)) && not(zoomScaleButton.IsIn(clickPoint)) && not(animButton.IsIn(clickPoint)) && not(scrollBtn.IsIn(clickPoint)) && trovato then
    let dialog = new OpenFileDialog()             //menu del file system per la selezione dell'immagine da inserire
    dialog.Title <- "Select an image"
    dialog.Filter <- "|*.bmp; .*gif; *.png; *.jpg; *.jpg; *.BMP; *.GIF; *.PNG; *.JPG; *.JPEG"
    dialog.ShowDialog() |> ignore
    let imgName = dialog.FileName                 //path dell'immagine da caricare
  
    if imgName <> "" then
         let img = Bitmap.FromFile(imgName)
                                                //ottengo in "provvMassCenter", se trovato=true, il centro di massa sul quale ho cliccato
         if trovato then                        //le coordinate del click sono situate in clickPoint (ins by OnMouseDown)

           if (provvMassCenter.getImage() <> null) then
                             provvMassCenter.getImage().Dispose()
           provvMassCenter.setImage(img)        //assegno l'immagine al centro di massa
           x.Invalidate()
   
 override x.OnMouseDown e =
  base.OnMouseDown e
  clickPoint <- new PointF( float32(e.Location.X), float32(e.Location.Y) )

  if zoomIncrButton.IsIn(clickPoint) then
                        x.ZoomIn()
                        x.Invalidate()

   elif zoomScaleButton.IsIn(clickPoint) then
                       x.ZoomOut()
                       x.Invalidate()
   
     elif animButton.IsIn(clickPoint) then
                       if not(timerAnimation.Enabled) then
                            animButton.setPressed(true) |> ignore
                            timerAnimation.Start()
                            else
                            animButton.setPressed(false) |> ignore
                            timerAnimation.Stop()
                            x.Invalidate()

       elif scrollBtn.IsIn(clickPoint) then
                        timerScroll.Start()
                        scrollBtn.setColor(Color.LightBlue, clickPoint)  

         else
            clickPoint.X <- (float32(e.Location.X) / zoomvalue) - traslationX
            clickPoint.Y <- (float32(e.Location.Y) / zoomvalue) - traslationY

            if not(check_postition(clickPoint)) then          //se non vi sono sovrapposizioni, disegno nuovo centro di massa
              let cm = new MassCenter()
              massList <- cm :: massList
              cm.setCenter(clickPoint)
              x.Invalidate()
                                                              //se vi è sovrapposizione, vuol dire che sto cliccando su un centro di massa per allargarlo
                  else                                       //a quale centro di massa appartiene il punto ottenuto dal "mouse down"??
                    printfn "Skipped"               
                    let trovato = search_massCenter()                                
                    if trovato then
                     resize <- true
                     this.Cursor <- Cursors.SizeWE

 
 override x.OnClientSizeChanged e =
    base.OnClientSizeChanged e
    x.Invalidate()

 override x.OnResize e =
    base.OnResize e
    x.Invalidate()
 
 override x.OnMouseMove e =
  base.OnMouseMove e
  //una volta trovato l'elemento(inserito in "provvMassCenter" da "OnMouseDown"), lo ridimensiono allungandogli  il raggio
  if resize then
    let diff = ((float32(e.Location.X) / zoomvalue) - traslationX) - clickPoint.X          //potenziale di ingrandimento raggio: diff tra punto di rilascio e punto di click, SOLO SU X
    clickPoint <- new PointF( (float32(e.Location.X) / zoomvalue) - traslationX, (float32(e.Location.Y) / zoomvalue) - traslationY )
    provvMassCenter.changeRay( provvMassCenter.getRay() + diff )                //modifico il raggio dell'elemento direttamente nella lista
    x.Invalidate()                                                              //ridisegno il tutto

 override x.OnMouseUp e =
  base.OnMouseUp e
  resize <- false                        //in caso di ridimensionamento, lo termina
  this.Cursor <- Cursors.Arrow
  if timerScroll.Enabled then
       timerScroll.Stop()
       scrollBtn.setColor(Color.DarkCyan, clickPoint)
       x.Invalidate()

 override x.OnPaint e =
  let g = e.Graphics
 
  g.ScaleTransform(zoomvalue, zoomvalue)
  g.TranslateTransform(traslationX, traslationY)

  let a = List.toArray massList

  if timerAnimation.Enabled then
     x.Animate(a)

  if massList <> [] then                    //ridisegno di tutti i centri di massa
        for i in 0 .. a.Length - 1 do
            a.[i].PaintCircle(g, a.[i].getCenter().X, a.[i].getCenter().Y) 

  g.ResetTransform()

  //bottoni
  zoomIncrButton.PaintButton(g, 0.f, 0.f, '+')
  zoomScaleButton.PaintButton(g, 0.f, 30.f, '-')
  animButton.PaintButton(g,0.f, float32(this.Height) - 30.f, '>')
  scrollBtn.PaintBtn(g, float32(this.Width) - 91.f, float32(this.Height) - 91.f)


let f = new Form(TopMost=true)
let cm = new Space()
cm.Dock <- DockStyle.Fill
f.Controls.Add(cm)
f.Show()