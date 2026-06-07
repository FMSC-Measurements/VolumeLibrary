/******************************************************************
 * File: PMTForm.cs
 * Author: T. Heithecker
 * Date: 10.08.2010
 * Revision Date: 10.29.10
 * Revision: Cleaned up code and commented.
 * To do: Add logic for comparing runs.
 * Purpose: This is the code behind the profile model visualization
 * Requirements: A tree with log/volume info.
 * ***************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace volCStest
{
    public partial class PMTForm : Form
    {
        float[,] dibs;
        float httot, dibScalingFactor;
        float imageTop = 750;
        float textTop = 5;
        float leftAnchor = 0;
        bool  isBehres = false;
        Graphics gfx;
        //SolidBrush redBrush;
        Bitmap image;
        string myFont = "Arial";
        int formHeight = 900;
        //Font drawFont = new Font("Calibri", 11);

        public PMTForm(float[,] dibsIn, float httotIn, float dibScalFact)
        {
            InitializeComponent();
            //this.Size = new System.Drawing.Size(1200,900);
            //changed to the following to display on laptop screen
            if (Screen.PrimaryScreen.WorkingArea.Height < formHeight) formHeight = Screen.PrimaryScreen.WorkingArea.Height;
            this.Size = new System.Drawing.Size(1200, formHeight);
            dibs = dibsIn;
            dibScalingFactor = dibScalFact;
            httot = httotIn;
            treePB.Width = this.ClientSize.Width;
            treePB.Height = this.ClientSize.Height;           
            
        }
        //constructor for r6 behres
        public PMTForm(float httotIn, float dibScalFact, bool isBehres_n)
        {
            InitializeComponent();
            //this.Size = new System.Drawing.Size(1200, 900);
            //changed to the following to display on laptop screen
            if (Screen.PrimaryScreen.WorkingArea.Height < formHeight) formHeight = Screen.PrimaryScreen.WorkingArea.Height;
            this.Size = new System.Drawing.Size(1200, formHeight);
            dibScalingFactor = dibScalFact;
            httot = httotIn;         
            isBehres = isBehres_n;
            treePB.Width = this.ClientSize.Width;
            treePB.Height = this.ClientSize.Height;    

        }


        private void PMTForm_Load(object sender, EventArgs e)
        {
            //create a bitmap to draw on...this will stay even on refresh
            image = new Bitmap(1200, 850, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            
            gfx = Graphics.FromImage(image);
            gfx.TextRenderingHint = System.Drawing.Text.TextRenderingHint.SingleBitPerPixelGridFit;
            if(!isBehres)
                drawTree();
        }

        //public void drawAnother (

        public Bitmap ShowForm()
        {
            //this.ShowDialog();
            this.Show();
            return this.image;
        }

        
        protected override void OnPaint(PaintEventArgs paintEvnt)
        {
            
        }

        public void drawTree()
        {
            //Graphics gfx = Graphics.FromImage(image);
            SolidBrush myBrush = new SolidBrush(Color.Green);
            //theoretical tree is 84 feet tall, dbh = 21"
            //trees will always be a max of 750 pixels tall
            //seg  length = 750/300(#number of points representing tree)
            float segLen = 2.5F;
            float curSeg = segLen;

            for (int i = 0; i < 300; i++)
            {
                gfx.FillEllipse(myBrush, (250 + dibScalingFactor * dibs[0, i]), imageTop - curSeg, 4, 4);
                gfx.FillEllipse(myBrush, (250 - dibScalingFactor * dibs[0, i]), imageTop - curSeg, 4, 4);
                
                curSeg += segLen;

                //******************************************************************
                //need to figure out why there's a bunch of 0's at the top of tree
                //but for now, just stop when dib = 0.0
                //******************************************************************
                if (dibs[0, i] == 0)
                    break;

            }
            //re paint the picture box
            treePB.Image = image;
            treePB.Invalidate();
        }

        public void drawTree(float[,] dibsIn, float httotIn, float dibScalFact)
        {

            System.Drawing.Brush myBrush;
            myBrush = new SolidBrush(Color.Green);
            Graphics g = Graphics.FromImage(image);

            Pen myPen = new Pen(Color.Red, 3);
            

            leftAnchor = 600;
            float[,] dibs2 = dibsIn;
            dibScalingFactor = dibScalFact;
            httot = httotIn;

            gfx.DrawLine(myPen, leftAnchor-50, 10.0F, leftAnchor-50, 810.0F);

            //theoretical tree is 84 feet tall, dbh = 21"
            //trees will always be a max of 750 pixels tall
            //seg  length = 750/300(#number of points representing tree)
            float segLen = 2.5F;
            float curSeg = segLen;

            for (int i = 0; i < 300; i++)
            {
                //the tree profile is 300 tiny ellipses all scaled to the tree size
                gfx.FillEllipse(myBrush, (850 + dibScalingFactor * dibs2[0, i]), imageTop - curSeg, 4, 4);
                gfx.FillEllipse(myBrush, (850 - dibScalingFactor * dibs2[0, i]), imageTop - curSeg, 4, 4);
                //dib -= .04F;
                curSeg += segLen;

                //******************************************************************
                //need to figure out why there's a bunch of 0's at the top of tree
                //but for now, just stop when dib = 0.0
                //******************************************************************
                if (dibs2[0, i] == 0)
                    break;

            }
            //repaint the picture box
            treePB.Image = image;
            treePB.Invalidate();
            myBrush.Dispose();
            g.Dispose();
            
        }

        public void drawLogs(float[,] LOGDIA, float[] LOGLEN, float[,] LOGVOL, float stump, float[] BOLHT, float trim)
        {
            SizeF recSize;
            PointF startPoint;
            float totalCuFtVol = 0;
            float totalBdFtVol = 0;
            
            System.Drawing.Pen myPen;
            myPen = new System.Drawing.Pen(System.Drawing.Color.Brown,3);
            //Graphics g = Graphics.FromImage(image);

            int i = 0;
            //current height on the bole. start at stump
            //stump ht (1.0) divide # pixels per foot
            float pixelsPerFoot = httot / 750;
            //need actual stump ht
            float curBoleHt = stump / pixelsPerFoot;
            float curHt = stump;
            bool top = true;

            //recSize = new SizeF(LOGDIA[1, i] * dibScalingFactor * 2, 30);
           //// startPoint = new PointF((leftAnchor + 252) - LOGDIA[1, i] * dibScalingFactor, imageTop - curBoleHt);
            //draw ellipse bounded by x,y, width height
            RectangleF myRectangle;// = new RectangleF(startPoint, recSize);
            //gfx.DrawEllipse(myPen, myRectangle);
            //drawString(LOGVOL[i, 3], 850 - curBoleHt, curHt);

            
            while (top)
            {
               
                i++;
                curBoleHt = curBoleHt + LOGLEN[i-1] / pixelsPerFoot;
                curHt += LOGLEN[i-1]+trim;

                recSize = new SizeF(LOGDIA[1, i] * dibScalingFactor * 2, (2/curHt * 100));
                startPoint = new PointF((leftAnchor + 252) - LOGDIA[1, i] * dibScalingFactor, imageTop - curBoleHt);
                //draw ellipse bounded by x,y, width height
                myRectangle = new RectangleF(startPoint, recSize);
                gfx.DrawEllipse(myPen, myRectangle);

                //drawString(LOGVOL[i - 1, 3], LOGVOL[i - 1, 0], imageTop - curBoleHt, curHt, LOGDIA[0, i], LOGDIA[1, i]);
                //drawLogLen(LOGLEN[i - 1], imageTop - (curBoleHt - LOGLEN[i-1] / pixelsPerFoot / 2));
                drawString2(imageTop - curBoleHt, curHt, LOGDIA[0, i], LOGDIA[1, i]);
                drawLogLen2(LOGVOL[i - 1, 3], LOGVOL[i - 1, 0], LOGLEN[i - 1], imageTop - (curBoleHt - LOGLEN[i - 1] / pixelsPerFoot / 2));

                totalCuFtVol += LOGVOL[i - 1, 3];
                totalBdFtVol += LOGVOL[i - 1, 0];

                if (i > 0)
                {
                    if (i >= 20) break;
                    if (LOGLEN[i] == 0) break;
                }
            }
            drawTotalVol(totalCuFtVol,totalBdFtVol);
           // drawTotalVol(totalBdFtVol, "Board Foot",50);
            treePB.Image = image;
            treePB.Invalidate();            
        }

        private void drawLogLen(float loglen, float ht)
        {
            String drawString = loglen.ToString();
            Font drawFont = new Font("Arial", 10);
            SolidBrush drawBrush = new SolidBrush(Color.Green);
            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 134.0F;
            float y = ht;

            // Set format of string.
            StringFormat drawFormat = new StringFormat();
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);
            drawFont.Dispose();
            drawBrush.Dispose();

        }
        //This to draw the log CUFT, BF and log Len on the same line
        private void drawLogLen2(float cuVol, float bdFtVol, float loglen, float ht)
        {
            //draw the log cuVol first
            String drawString = Math.Round(cuVol, 1).ToString();
            Font drawFont = new Font("Arial", 10);
            SolidBrush drawBrush = new SolidBrush(Color.Black);
            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 10.0F;
            float y = ht;
            // Set format of string draw volume
            StringFormat drawFormat = new StringFormat();
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change color and move over to draw bd ft volume
            drawString = Math.Round(bdFtVol).ToString();
            x = leftAnchor + 70.0F;
            drawBrush.Color = Color.Brown;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //then draw Log Lenth
            drawString = loglen.ToString();
            x = leftAnchor + 134.0F;
            drawBrush.Color = Color.Green;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);
            drawFont.Dispose();
            drawBrush.Dispose();

            //SolidBrush drawBrush = new SolidBrush(Color.Green);
            // Create point for upper-left corner of drawing.
            //float x = leftAnchor + 134.0F;
            //float y = ht;

            // Set format of string.
            //StringFormat drawFormat = new StringFormat();
            //gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

        }

        private void drawString(float cuVol, float bdFtVol, float ht, float curHt, float scalDib, float dib)
        {
            // Create string to draw.
//            String drawString = "Cubic vol = " + vol +"\nht = " + curHt;
            String drawString = Math.Round(cuVol, 2).ToString();
           // String drawString2 = "     " + curHt;
            // Create font and brush.
            Font drawFont = new Font("Arial", 10);
            SolidBrush drawBrush = new SolidBrush(Color.Black);
            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 10.0F;
            float y = ht;
            
            // Set format of string draw volume
            StringFormat drawFormat = new StringFormat();
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change color and move over to draw bd ft volume
            drawString = bdFtVol.ToString();
            x = leftAnchor + 70.0F;
            drawBrush.Color = Color.Brown;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);


            //change colors and move over to draw height
            drawString = curHt.ToString();
            x = leftAnchor + 340.0F;
            drawBrush.Color = Color.Red;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change colors and move over to draw dibs scaled
            drawString = scalDib.ToString();
            x = leftAnchor + 410.0F;
            drawBrush.Color = Color.Blue;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change colors and move over to draw dibs
            //drawString = "";
            drawString = Math.Round(dib,2).ToString();
            x = leftAnchor + 470.0F;
            //drawBrush.Color = Color.Blue;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);
            drawFont.Dispose();
            drawBrush.Dispose();


        }
        //This method only draw the height and dib.
        private void drawString2(float ht, float curHt, float scalDib, float dib)
        {
            // Create string to draw.
            String drawString = curHt.ToString();
            // Create font and brush.
            Font drawFont = new Font("Arial", 10);
            SolidBrush drawBrush = new SolidBrush(Color.Black);
            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 10.0F;
            float y = ht;

            // Set format of string draw volume
            StringFormat drawFormat = new StringFormat();

            //change colors and move over to draw height
            drawString = curHt.ToString();
            x = leftAnchor + 340.0F;
            drawBrush.Color = Color.Red;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change colors and move over to draw dibs scaled
            drawString = scalDib.ToString();
            x = leftAnchor + 410.0F;
            drawBrush.Color = Color.Blue;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //change colors and move over to draw dibs
            //drawString = "";
            drawString = Math.Round(dib, 2).ToString();
            x = leftAnchor + 470.0F;
            //drawBrush.Color = Color.Blue;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawFont.Dispose();
            drawBrush.Dispose();

        }


        private void drawTotalVol(float cuvol, float bdftvol)
        {
            // Create string to draw.
            String drawString = Math.Round(cuvol,1).ToString();
            // Create font and brush.
            Font drawFont = new Font("Arial", 10);
            SolidBrush drawBrush = new SolidBrush(Color.Black);
            
            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 10.0F;
            float y = 780.0F;

            // Set format of string.
            StringFormat drawFormat = new StringFormat();
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            //move a little right and draw board foot volume
            x = leftAnchor + 70.0F;
            drawBrush.Color = Color.Black;
            drawString = Math.Round(bdftvol).ToString();
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = " Cubic" + "\nVolume";
            x = leftAnchor + 1;
            y = textTop;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = " Bd Ft" + "\nVolume";
            x = leftAnchor + 62;
            y = textTop;
            drawBrush.Color = Color.Brown;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = "  Log" + "\nLength";
            x = leftAnchor + 126;
            y = textTop;
            drawBrush.Color = Color.Green;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = "Height";
            drawBrush.Color = Color.Red;
            x = leftAnchor + 330;
            y = textTop;

            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = "  DIB" + "\nScaled";
            drawBrush.Color = Color.Blue;
            x = leftAnchor + 390;
            y = textTop;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = " DIB" + "\nActual";
            drawBrush.Color = Color.Blue;
            x = leftAnchor + 460;
            y = textTop;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = " Sum" + "\nMerch";
            drawBrush.Color = Color.Black;
            x = leftAnchor + 5.0F;
            y = 740.0F;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawString = " Sum" + "\nMerch";
            drawBrush.Color = Color.Brown;
            x = leftAnchor + 62.0F;
            y = 740.0F;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawFont.Dispose();
            drawBrush.Dispose();

        }

        public void drawBehresTree(float[,] LOGDIA, float[] LOGLEN, float[,] LOGVOL, float stump, float[] BOLHT, float anchorIn)
        {
            PointF lowerLeftPt, lowerRightPt, upperLeftPt, upperRightPt;
            
            leftAnchor = anchorIn;

            System.Drawing.Pen myPen;
            myPen = new System.Drawing.Pen(System.Drawing.Color.Red, 3);
            if (leftAnchor > 0)
                gfx.DrawLine(myPen, leftAnchor - 50, 10.0F, leftAnchor - 50, 810.0F);

            myPen.Color = Color.Green;
            int i = 0;
            //current height on the bole. start at stump
            //stump ht (1.0) divide # pixels per foot
            float pixelsPerFoot = httot / 750;
            //need actual stump ht
            //stump = 0;// -.45F;
            float curBoleHt =  stump / pixelsPerFoot;
            float curHt = stump;
           // bool top = true;

            //get points for bottom of log
            //recSize = new SizeF(LOGDIA[1, i] * dibScalingFactor * 2, 10);
            lowerLeftPt = new PointF((leftAnchor + 252) - LOGDIA[0, i] * dibScalingFactor, imageTop - curBoleHt);
            lowerRightPt = new PointF((leftAnchor + 252) + LOGDIA[0, i] * dibScalingFactor, imageTop - curBoleHt);
            curBoleHt = curBoleHt + LOGLEN[i] / pixelsPerFoot;
            upperLeftPt = new PointF ((leftAnchor + 252) - LOGDIA[0, i+1] * dibScalingFactor, imageTop - curBoleHt);
            upperRightPt = new PointF((leftAnchor + 252) + LOGDIA[0, i + 1] * dibScalingFactor, imageTop - curBoleHt);
            
            /*PointF[] curvePoints = 
             {
                 upperLeftPt,
                 lowerLeftPt,
                 lowerRightPt,
                 
                 upperRightPt
                 
             };*/


            //draw ellipse bounded by x,y, width height
            //RectangleF myRectangle = new RectangleF(startPoint, recSize);
            //gfx.DrawPolygon(myPen, curvePoints);
            //drawString(LOGVOL[i, 3], 850 - curBoleHt, curHt);

            gfx.DrawLine(myPen, lowerLeftPt, upperLeftPt);
            gfx.DrawLine(myPen, lowerRightPt, upperRightPt);



            while (LOGDIA[0, i+2] != 0)
            {
                lowerLeftPt = upperLeftPt;
                lowerRightPt = upperRightPt;
                i++;
                curBoleHt = curBoleHt + LOGLEN[i - 1] / pixelsPerFoot;
                curHt += LOGLEN[i - 1];

                upperLeftPt = new PointF((leftAnchor + 252) - LOGDIA[0, i + 1] * dibScalingFactor, imageTop - curBoleHt);
                upperRightPt = new PointF((leftAnchor + 252) + LOGDIA[0, i + 1] * dibScalingFactor, imageTop - curBoleHt);

                gfx.DrawLine(myPen, lowerLeftPt, upperLeftPt);
                gfx.DrawLine(myPen, lowerRightPt, upperRightPt);
                /*
                recSize = new SizeF(LOGDIA[1, i] * dibScalingFactor * 2, (2 / curHt * 100));
                startPoint = new PointF((leftAnchor + 252) - LOGDIA[0, i] * dibScalingFactor, imageTop - curBoleHt);
                //draw ellipse bounded by x,y, width height
                myRectangle = new RectangleF(startPoint, recSize);
                gfx.DrawEllipse(myPen, myRectangle);

                drawString(LOGVOL[i - 1, 3], LOGVOL[i - 1, 0], imageTop - curBoleHt, curHt, LOGDIA[0, i], LOGDIA[1, i]);
                drawLogLen(LOGLEN[i - 1], imageTop - (curBoleHt - LOGLEN[i - 1] / pixelsPerFoot / 2));

                totalCuFtVol += LOGVOL[i - 1, 3];
                totalBdFtVol += LOGVOL[i - 1, 0];

                if (i > 0)
                    if (LOGLEN[i] == 0) break;
            }
            drawTotalVol(totalCuFtVol, totalBdFtVol);
            // drawTotalVol(totalBdFtVol, "Board Foot",50);
            */
            }
            treePB.Invalidate();
        }

        public void drawTreeInfo(float dbh, float tht, string voleq, float mtopp, float stump, float trim, float maxlen, float minlen, int opt, float anchorIn)
        {
            // Create string to draw.
            String drawString1 = voleq + "   DBH: " + Math.Round(dbh, 1).ToString() + "   THT: " + Math.Round(tht, 1).ToString();
            String drawString2 = "MTOPP: " + Math.Round(mtopp, 1).ToString() + "   STUMP: " + Math.Round(stump, 1).ToString() + "   TRIM: " + Math.Round(trim, 1).ToString();
            String drawString3 = "MAXLEN: " + Math.Round(maxlen, 1).ToString() + "   MINLEN: " + Math.Round(minlen, 1).ToString() + "   OPT: " + opt.ToString();
            String drawString = drawString1 + "\n" + drawString2 + "\n" + drawString3;
            // Create font and brush.
            Font drawFont = new Font(myFont, 10);
            SolidBrush drawBrush = new SolidBrush(Color.Black);

            // Create point for upper-left corner of drawing.
            float x = leftAnchor + 180.0F;
            float y = 780.0F;

            // Set format of string.
            StringFormat drawFormat = new StringFormat();
            //gfx.TextRenderingHint = System.Drawing.Text.TextRenderingHint.SingleBitPerPixelGridFit;
            gfx.DrawString(drawString, drawFont, drawBrush, x, y, drawFormat);

            drawFont.Dispose();
            drawBrush.Dispose();
            //test drawing text...
            //testDrawText();
        }
        private void testDrawText()
        {
            // Retrieve the graphics object.
            Graphics formGraphics = gfx;

            // Declare a new font.
            Font myFont = new Font(FontFamily.GenericSansSerif, 10,
                FontStyle.Regular);

            // Set the TextRenderingHint property.
            formGraphics.TextRenderingHint =
                System.Drawing.Text.TextRenderingHint.SingleBitPerPixel;

            // Draw the string.
            formGraphics.DrawString("Hello World", myFont,
                Brushes.Firebrick, 20.0F, 60.0F);

            // Change the TextRenderingHint property.
            formGraphics.TextRenderingHint =
                System.Drawing.Text.TextRenderingHint.AntiAliasGridFit;

            // Draw the string again.
            formGraphics.DrawString("Hello World", myFont,
                Brushes.Firebrick, 20.0F, 100.0F);

            // Set the text contrast to a high-contrast setting.
            formGraphics.TextContrast = 0;

            // Draw the string.
            formGraphics.DrawString("Hello World", myFont,
                Brushes.DodgerBlue, 20.0F, 140.0F);

            // Set the text contrast to a low-contrast setting.
            formGraphics.TextContrast = 12;

            // Draw the string again.
            formGraphics.DrawString("Hello World", myFont,
                Brushes.DodgerBlue, 20.0F, 180.0F);

            // Dispose of the font object.
            myFont.Dispose();

        }
        private void treePB_Paint(object sender, PaintEventArgs e)
        {
            e.Graphics.DrawImage(image, 0, 0, image.Width, image.Height);
            
        }

        private void button1_Click_1(object sender, EventArgs e)
        {
            treePB.Image = null;
            gfx.Clear(this.BackColor);           
        }

       
    }
}

 