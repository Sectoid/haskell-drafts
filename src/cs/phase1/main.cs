// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Text;
using System.Diagnostics;

namespace Language.Haskell.Phase1
{

public class Hsc
{
  public static int Main(string[] args)
  {
    if(args.Length < 1)
    {
      Console.WriteLine("Not enough arguments. Use hsc.exe <file-to-compile> instead.");
      return 1;
    }

    var input = args[0];
    
    Process parser = new Process();
    parser.StartInfo.UseShellExecute = false;
    parser.StartInfo.FileName = "Parser";
    parser.StartInfo.Arguments = input;
    parser.StartInfo.RedirectStandardOutput = true;
    parser.StartInfo.RedirectStandardError = true;

    parser.Start();
    parser.WaitForExit();

    if(parser.ExitCode != 0)
    {
      // var output = parser.StandardOutput.
      Console.WriteLine("Parser finished with error:");
      string line;
      while ((line = parser.StandardError.ReadLine()) != null)
      {
	Console.WriteLine(line);
      }
      return 1;
    }

    var loader = new DOMLoader();

    var module = loader.load(parser.StandardOutput);

    Console.WriteLine("Successfully parsed! {0}", module);

    return 0;
  }
}

}
