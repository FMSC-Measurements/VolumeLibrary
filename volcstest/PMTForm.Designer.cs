namespace volCStest
{
    partial class PMTForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(PMTForm));
            this.treePB = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.treePB)).BeginInit();
            this.SuspendLayout();
            // 
            // treePB
            // 
            this.treePB.Location = new System.Drawing.Point(1, 21);
            this.treePB.Margin = new System.Windows.Forms.Padding(1);
            this.treePB.Name = "treePB";
            this.treePB.Size = new System.Drawing.Size(459, 429);
            this.treePB.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.treePB.TabIndex = 0;
            this.treePB.TabStop = false;
            this.treePB.Paint += new System.Windows.Forms.PaintEventHandler(this.treePB_Paint);
            // 
            // PMTForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.ClientSize = new System.Drawing.Size(553, 466);
            this.Controls.Add(this.treePB);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "PMTForm";
            this.Text = "PMTForm";
            this.Load += new System.EventHandler(this.PMTForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.treePB)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.PictureBox treePB;

    }
}