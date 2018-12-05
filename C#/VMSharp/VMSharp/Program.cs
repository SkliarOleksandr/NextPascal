using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VM;

namespace VMSharp
{

    using Pointer = UInt32;

    class Program
    {



        static void Main(string[] args)
        {
            Console.WriteLine("==========================");
            Console.WriteLine("======  VM C# TEST  ======");
            Console.WriteLine("==========================");
           
            VMachine machine = new VMachine();
            try
            {
                machine.LoadVMImage("image.bin");
                machine.Run();
            } catch (Exception e)
            {
                Console.WriteLine("VM image load ERROR: " + e.Message);
            }
            Console.WriteLine("Done. Press any key...");
            Console.Read();
        }
    }
}
