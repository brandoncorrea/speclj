namespace mmargs;

using System.Text;
using System.Text.RegularExpressions;

public class Arguments
{
    public static readonly int MAX_ROW_LENGTH = 72;
    private ICollection<Parameter> parameters = new LinkedList<Parameter>();
    private ICollection<Option> options = new LinkedList<Option>();

    public void addParameter(string name, string description)
    {
        parameters.Add(new Parameter(name, description, true, false));
    }

    public void addOptionalParameter(string name, string description)
    {
        parameters.Add(new Parameter(name, description, false, false));
    }

    public void addMultiParameter(string name, string description)
    {
        parameters.Add(new Parameter(name, description, false, true));
    }

    public void addSwitchOption(string shortName, string fullName, string description)
    {
        addValueOption(shortName, fullName, null, description);
    }

    public void addValueOption(string shortName, string fullName, string valueDescription, string description)
    {
        addValueOption(shortName, fullName, valueDescription, description, false);
    }

    private void addValueOption(string shortName, string fullName, string valueDescription, string description, bool multi)
    {
      if (shortName == null || fullName == null)
          throw new SystemException("Options require a shortName and fullName");
      options.Add(new Option(shortName, fullName, valueDescription, description, multi));
    }

    public void addMultiOption(String shortName, String fullName, String valueDescription, String description)
    {
        addValueOption(shortName, fullName, valueDescription, description, true);
    }

    public Dictionary<string, object> parse(params string[] args)
    {
        var results = new Dictionary<string, object>();
        LinkedList<string> parameters = ParseOptions(args, results);
        LinkedList<string> leftOver = ParseParameters(parameters, results);
        ProcessLeftOver(leftOver, results);
        if (leftOver.Count > 0)
            results["*leftover"] = leftOver;
        return results;
    }

    public String argString()
    {
        var sb = new StringBuilder();
        if (options.Count > 0)
            sb.Append("[options] ");

        for (int i = 0; i < parameters.Count; i++)
        {
            if (i > 0)
                sb.Append(" ");
            Parameter parameter = parameters.ElementAt(i);
            if (parameter.required)
                sb.Append("<").Append(parameter.name).Append(">");
            else
                sb.Append("[").Append(parameter.name).Append(parameter.multi ? "*" : "").Append("]");
        }

        return sb.ToString();
    }

    public string parametersString()
    {
      string[] names = new string[parameters.Count];
      string[] descriptions = new string[parameters.Count];
      for (int i = 0; i < parameters.Count; i++)
      {
        Parameter parameter = parameters.ElementAt(i);
        names[i] = parameter.name;
        descriptions[i] = parameter.description;
      }
      return Tabularize(names, descriptions);
    }

    public string optionsString()
    {
        string[] heads = new string[options.Count];
        string[] descriptions = new string[options.Count];
        for (int i = 0; i < options.Count; i++)
        {
            Option option = options.ElementAt(i);
            heads[i] = option.Head();
            descriptions[i] = option.description;
        }
        return Tabularize(heads, descriptions);
    }

    public Option FindOption(String name)
    {
        foreach (Option option in options)
        {
            if (option.shortName.Equals(name) || option.fullName.Equals(name))
                return option;
        }
        return null;
    }

    private LinkedList<string> ParseParameters(LinkedList<string> args, Dictionary<string, object> results)
    {
        var leftOver = new LinkedList<string>();
        var unfilledParams = new LinkedList<Parameter>(this.parameters);

        Parameter parameter = null;
        while(args.Any())
        {
            string arg = Pop(args);
            if (IsOption(arg))
                leftOver.AddLast(arg);
            else
            {
                if (unfilledParams.Any())
                {
                    parameter = Pop(unfilledParams);
                    SetValue(results, parameter.name, arg, parameter.multi);
                }
                else
                {
                    if(parameter != null && parameter.multi)
                        SetValue(results, parameter.name, arg, true);
                    else
                        leftOver.AddLast(arg);
                }
            }
        }

        foreach (Parameter unfilledParam in unfilledParams)
            if (unfilledParam.required)
                AddError(results, "Missing parameter: " + unfilledParam.name);

        return leftOver;
    }

    private void SetValue(Dictionary<string, object> results, string name, string arg, bool multi)
    {
        if(multi)
        {
            var values = (ICollection<string>) GetKey(results, name);
            if(values == null)
            {
                values = (ICollection<string>) new LinkedList<string>();
                results[name] = values;
            }
            values.Add(arg);
        }
        else
            results[name] = arg;
    }

    private object GetKey(Dictionary<string, object> results, string key) =>
      results.ContainsKey(key) ? results[key] : null;

    private static T Pop<T>(LinkedList<T> args) where T : class
    {
        if (args.Any())
        {
            var first = args.First.Value;
            args.RemoveFirst();
            return first;
        }
        return null;
    }

    private LinkedList<string> ParseOptions(string[] argArray, Dictionary<string, object> results)
    {
        var args = new LinkedList<string>(argArray);
        var leftOver = (ICollection<string>)new LinkedList<string>();
        while (args.Any())
        {
            string arg = Pop(args);
            if (IsOption(arg))
            {
                OptionParser parser = new OptionParser(arg);
                if (FindOption(parser.argName) != null)
                    ParseOption(parser, args, results);
                else
                    leftOver.Add(arg);
            }
            else
                leftOver.Add(arg);
        }
        return (LinkedList<string>)leftOver;
    }

    private void ProcessLeftOver(LinkedList<string> leftOver, Dictionary<string, object> results)
    {
        foreach (string arg in leftOver)
        {
            if(IsOption(arg))
                AddError(results, "Unrecognized option: " + arg);
            else
                AddError(results, "Unexpected parameter: " + arg);
        }
    }

    private void AddError(Dictionary<string, object> results, string message)
    {
        var errors = (LinkedList<string>) GetKey(results, "*errors");
        if (errors == null)
        {
          errors = new LinkedList<string>();
          results["*errors"] = errors;
        }
        errors.AddFirst(message);
    }

    private void ParseOption(OptionParser parser, LinkedList<string> args, Dictionary<string, object> results)
    {
        Option option = FindOption(parser.argName);
        if (option.RequiresValue())
        {
          if (parser.usingEquals)
          {
            if (parser.argValue == null)
              AddError(results, "Missing value for option: " + parser.argName);
            SetValue(results, option.fullName, parser.argValue, option.multi);
          }
          else
          {
            string nextArg = args.Any() ? args.First.Value : null;
            if (nextArg == null || IsOption(nextArg))
              AddError(results, "Missing value for option: " + parser.argName);
            SetValue(results, option.fullName, nextArg, option.multi);
            Pop(args);
          }
        }
        else
          results[option.fullName] = "on";
    }

    private bool IsOption(string arg) => arg.StartsWith("-");

    public static string Tabularize(string[] col1, string[] col2)
    {
        int maxLength = 0;
        foreach(string s in col1)
            if(s.Length > maxLength)
                maxLength = s.Length;

        var sb = new StringBuilder();
        for(int i = 0; i < col1.Length; i++)
        {
            sb.Append("  ").Append(col1[i]);
            int remainingSpaces = maxLength - col1[i].Length;
            AppendSpaces(sb, remainingSpaces + 2);
            var lines = SplitIntoLines(col2[i]);
            sb.Append(Pop(lines));
            sb.Append(Environment.NewLine);
            while(lines.Any())
            {
                AppendSpaces(sb, maxLength + 4);
                sb.Append(Pop(lines));
                sb.Append(Environment.NewLine);
            }
        }
        return sb.ToString();
    }

    private static string NewlineRegex = "\\r\\n|\\n";

    private static LinkedList<string> SplitIntoLines(string value)
    {
        string[] lines = Regex.Split(value, NewlineRegex, RegexOptions.Multiline);
        var measuredLines = new LinkedList<string>();
        foreach (string line in lines)
        {
            var measuredLine = line;
            while(measuredLine.Length > MAX_ROW_LENGTH)
            {
                int splitIndex = FindIndexOfSpaceBefore(MAX_ROW_LENGTH, line);
                measuredLines.AddLast(line.Substring(0, splitIndex));
                measuredLine = line.Substring(splitIndex + 1);
            }
            measuredLines.AddLast(measuredLine);
        }
        return measuredLines;
    }

    private static int FindIndexOfSpaceBefore(int end, string line)
    {
        for(int i = end; i > 0; i--)
            if (line[i] == ' ')
                return i;
        return MAX_ROW_LENGTH;
    }

    private static void AppendSpaces(StringBuilder buffer, int spaces)
    {
        for(int j = 0; j < spaces; j++)
          buffer.Append(" ");
    }

    public class Parameter
    {
        internal string name;
        internal bool required;
        internal string description;
        internal bool multi;

        public Parameter(string name, string description, bool required, bool multi)
        {
          this.name = name;
          this.description = description;
          this.required = required;
          this.multi = multi;
        }
    }

    public class Option
    {
        internal string shortName;
        internal string fullName;
        internal string valueDescription;
        internal string description;
        internal string head;
        internal bool multi;

        public Option(string shortName, string fullName, string valueDescription, string description, bool multi)
        {
          this.shortName = shortName;
          this.fullName = fullName;
          this.valueDescription = valueDescription;
          this.description = description;
          this.multi = multi;
        }

        public bool RequiresValue()
        {
          return valueDescription != null;
        }

        internal string Head()
        {
          if(head == null)
          {
            head = "-" + shortName + ", --" + fullName;
            if(RequiresValue())
              head += "=<" + valueDescription + ">";
          }
          return head;
        }
    }

    private class OptionParser
    {
        public string argName;
        public string argValue;
        public bool usingFullName;
        public bool usingEquals;

        public OptionParser(string arg)
        {
            StripDashes(arg);
            if (usingFullName)
            {
                int valueIndex = argName.IndexOf("=");
                if (valueIndex > 0)
                {
                    usingEquals = true;
                    argValue = argName.Substring(valueIndex + 1);
                    argName = argName.Substring(0, valueIndex);
                }
            }
        }

        private void StripDashes(string arg)
        {
            if (arg.StartsWith("--"))
            {
              usingFullName = true;
              argName = arg.Substring(2);
            }
            else
            {
              usingFullName = false;
              argName = arg.Substring(1);
            }
        }
    }
}
