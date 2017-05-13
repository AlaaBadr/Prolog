using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using SbsSW.SwiPlCs;

namespace Dots_and_Boxes
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            Environment.SetEnvironmentVariable("Path", @"C:\\Program Files\\swipl\\bin");
            string[] p = { "-q", "-f", @"Dots and Boxes.pl" };
            PlEngine.Initialize(p);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            richTextBox1.Clear();
            PlQuery load = new PlQuery("consult(\"Dots and Boxes.pl\")");
            load.NextSolution();

            PlQuery query = new PlQuery("opp('A',X)");
            foreach(PlQueryVariables v in query.SolutionVariables)
            {
                richTextBox1.Text = richTextBox1.Text + v + "\n";
            }
        }
    }
}
