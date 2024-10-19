namespace mmargstest;

using mmargs;
using NUnit;

public class ArgumentsTest
{
    private Arguments args;
    private Dictionary<string, object> results;

    [SetUp]
    public void Setup()
    {
        args = new Arguments();
    }

    [Test]
    public void ParsingNothing()
    {
        results = args.Parse();
        Assert.That(results, Is.Empty);
    }

    [Test]
    public void UnexpectedParameter()
    {
        CheckParseError("Unexpected parameter: foo", "foo");
        CheckParseError("Unexpected parameter: bar", "bar");
    }

    [Test]
    public void ParsingOneParameter()
    {
        args.AddParameter("foo", "Some Description");

        results = args.Parse("bar");
        Assert.That(results["foo"], Is.EqualTo("bar"));

        results = args.Parse("fizz");
        Assert.That(results["foo"], Is.EqualTo("fizz"));
    }

    [Test]
    public void MissingParameter()
    {
        args.AddParameter("foo", "Some Description");
        CheckParseError("Missing parameter: foo");
    }

    [Test]
    public void ParsingTwoParameters()
    {
        args.AddParameter("foo", "Some Description");
        args.AddParameter("bar", "Some Description");

        results = args.Parse("fizz", "bang");

        Assert.That(results["foo"], Is.EqualTo("fizz"));
        Assert.That(results["bar"], Is.EqualTo("bang"));
    }

    [Test]
    public void MissingOneOfTwoParameters()
    {
        args.AddParameter("foo", "Some Description");
        args.AddParameter("bar", "Some Description");

        CheckParseError("Missing parameter: foo");
        CheckParseError("Missing parameter: bar", "fizz");
    }

    [Test]
    public void OptionalParameter()
    {
        args.AddOptionalParameter("foo", "Some Description");

        results = args.Parse();
        Assert.That(results.ContainsKey("foo"), Is.False);
        Assert.That(results.ContainsKey("*errors"), Is.False);

        results = args.Parse("fizz");
        Assert.That(results["foo"], Is.EqualTo("fizz"));
        Assert.That(results.ContainsKey("*errors"), Is.False);
    }

    [Test]
    public void OneSwitchOption()
    {
        args.AddSwitchOption("m", "my-option", "my test option");

        results = args.Parse();
        Assert.That(results.ContainsKey("my-option"), Is.False);

        results = args.Parse("-m");
        Assert.That(results["my-option"], Is.EqualTo("on"));

        results = args.Parse("--my-option");
        Assert.That(results["my-option"], Is.EqualTo("on"));
    }

    [Test]
    public void TwoSwitchOptions()
    {
        args.AddSwitchOption("a", "a-option", "Option A");
        args.AddSwitchOption("b", "b-option", "Option B");

        results = args.Parse();
        Assert.That(results.ContainsKey("a-option"), Is.False);
        Assert.That(results.ContainsKey("b-option"), Is.False);

        results = args.Parse("-a");
        Assert.That(results.ContainsKey("a-option"), Is.True);
        Assert.That(results.ContainsKey("b-option"), Is.False);

        results = args.Parse("--b-option");
        Assert.That(results.ContainsKey("a-option"), Is.False);
        Assert.That(results.ContainsKey("b-option"), Is.True);

        results = args.Parse("--a-option", "-b");
        Assert.That(results.ContainsKey("a-option"), Is.True);
        Assert.That(results.ContainsKey("b-option"), Is.True);
    }
    
    [Test]
    public void OptionNamesAreRequired()
    {
        CheckOptionError("Options require a shortName and fullName", "a", null, null);
        CheckOptionError("Options require a shortName and fullName", null, "a-option", null);
        args.AddSwitchOption("a", "a-option", null);
    }

    [Test]
    public void UnrecognizedOption()
    {
        CheckParseError("Unrecognized option: -a", "-a");
        CheckParseError("Unrecognized option: --a-option", "--a-option");
    }

    [Test]
    public void OneValueOption()
    {
        args.AddValueOption("a", "a-option", "value", "Option A");

        results = args.Parse("-a", "value");
        Assert.That(results["a-option"], Is.EqualTo("value"));

        results = args.Parse("--a-option=value");
        Assert.That(results["a-option"], Is.EqualTo("value"));
    }

    [Test]
    public void MissingOptionValue()
    {
        args.AddValueOption("a", "a-option", "value", "Option A");

        CheckParseError("Missing value for option: a", "-a");
        CheckParseError("Missing value for option: a-option", "--a-option");
    }

    [Test]
    public void MissingOptionValueWhenFollowedByOption()
    {
        args.AddValueOption("a", "a-option", "value", "Option A");
        args.AddSwitchOption("b", "b-option", "Option B");

        CheckParseError("Missing value for option: a", "-a", "-b");
        CheckParseError("Missing value for option: a", "-a", "--b-option");
    }

    [Test]
    public void ParameterWithSwitchOption()
    {
        args.AddParameter("param", "Some Description");
        args.AddSwitchOption("a", "a-option", "Option A");

        CheckParseError("Missing parameter: param");
        CheckParseError("Missing parameter: param", "-a");
        CheckParseError("Missing parameter: param", "--a-option");

        results = args.Parse("-a", "blah");
        Assert.That(results["a-option"], Is.EqualTo("on"));
        Assert.That(results["param"], Is.EqualTo("blah"));

        results = args.Parse("--a-option", "blah");
        Assert.That(results["a-option"], Is.EqualTo("on"));
        Assert.That(results["param"], Is.EqualTo("blah"));
    }

    [Test]
    public void ParameterWithValueOption()
    {
        args.AddParameter("param", "Some Description");
        args.AddValueOption("a", "a-option", "value", "Option A");

        CheckParseError("Missing parameter: param");
        CheckParseError("Missing value for option: a", "-a");
        CheckParseError("Missing parameter: param", "-a", "foo");
        CheckParseError("Missing parameter: param", "--a-option=foo");

        results = args.Parse("-a", "foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));

        results = args.Parse("--a-option=foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));
    }

    [Test]
    public void ParameterOptionsAreParsableInLongFormWithoutEqualsSign()
    {
        args.AddParameter("param", "Some Description");
        args.AddValueOption("a", "a-option", "value", "Option A");

        results = args.Parse("--a-option", "foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));
    }

    [Test]
    public void RemainingArgs()
    {
        results = args.Parse("foo");
        AssertListsEquals(list("foo"), results["*leftover"]);

        args.AddParameter("param", "Some Description");
        results = args.Parse("foo", "bar");
        AssertListsEquals(list("bar"), results["*leftover"]);

        args.AddSwitchOption("a", "a-option", "Option A");
        results = args.Parse("-a", "foo", "bar");
        AssertListsEquals(list("bar"), results["*leftover"]);

        results = args.Parse("-z", "foo", "bar");
        AssertListsEquals(list("-z", "bar"), results["*leftover"]);
    }

    [Test]
    public void RemainingArgsWithValueOption()
    {
        args.AddParameter("param", "Some Description");
        args.AddValueOption("a", "a-option", "value", "Option A");

        results = args.Parse("-z");
        AssertListsEquals(list("-z"), results["*leftover"]);

        results = args.Parse("-z", "foo", "bar");
        AssertListsEquals(list("-z", "bar"), results["*leftover"]);

        results = args.Parse("-a", "foo", "bar", "fizz");
        AssertListsEquals(list("fizz"), results["*leftover"]);
    }

    [Test]
    public void CanParseOptionsMixedInWithParameters()
    {
        args.AddParameter("param1", "Some Description");
        args.AddParameter("param2", "Some Description");
        args.AddSwitchOption("a", "a-switch", "Switch A");
        args.AddValueOption("b", "b-option", "B", "Option B");
        args.AddValueOption("c", "c-option", "C", "Option C");

        results = args.Parse("-a", "one", "--b-option=two", "three", "--c-option", "four", "five");

        Assert.That(results["a-switch"], Is.EqualTo("on"));
        Assert.That(results["param1"], Is.EqualTo("one"));
        Assert.That(results["b-option"], Is.EqualTo("two"));
        Assert.That(results["param2"], Is.EqualTo("three"));
        Assert.That(results["c-option"], Is.EqualTo("four"));
    }

    [Test]
    public void MultiParameters()
    {
        args.AddMultiParameter("colors", "Any number of colors");

        results = args.Parse("red", "orange", "yellow");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red", "orange", "yellow"), results["colors"]);

        results = args.Parse();
        Assert.That(results.ContainsKey("*errors"), Is.False);
        Assert.That(results.ContainsKey("colors"), Is.False);

        results = args.Parse("red");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red"), results["colors"]);
    }

    [Test]
    public void multiOptions()
    {
        args.AddMultiOption("c", "color", "COLOR", "Some colors");

        results = args.Parse();
        Assert.That(results.ContainsKey("*errors"), Is.False);
        Assert.That(results.ContainsKey("color"), Is.False);

        results = args.Parse("-c", "red");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red"), results["color"]);

        results = args.Parse("-c", "red", "--color", "orange", "--color=yellow");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red", "orange", "yellow"), results["color"]);
    }

    [Test]
    public void ArgString()
    {
        Assert.That(args.ArgString(), Is.EqualTo(""));

        args.AddParameter("param", "Some Description");
        Assert.That(args.ArgString(), Is.EqualTo("<param>"));

        args.AddSwitchOption("a", "a-option", "Option A");
        Assert.That(args.ArgString(), Is.EqualTo("[options] <param>"));

        args.AddParameter("another-param", "Some Description");
        Assert.That(args.ArgString(), Is.EqualTo("[options] <param> <another-param>"));

        args.AddOptionalParameter("param3", "Parameter 3");
        Assert.That(args.ArgString(), Is.EqualTo("[options] <param> <another-param> [param3]"));

        args.AddMultiParameter("param4", "Parameter 4");
        Assert.That(args.ArgString(), Is.EqualTo("[options] <param> <another-param> [param3] [param4*]"));
    }

    [Test]
    public void ArgStringWithOptionalParameter()
    {
        args.AddOptionalParameter("param", "Some Description");
        Assert.That(args.ArgString(), Is.EqualTo("[param]"));
    }

    [Test]
    public void ParametersString()
    {
        Assert.That(args.ParametersString(), Is.EqualTo(""));

        args.AddParameter("foo", "Foo Param");
        Assert.That(args.ParametersString(), Is.EqualTo("  foo  Foo Param\n"));

        args.AddParameter("fizz", "Fizz Param");
        Assert.That(args.ParametersString(), Is.EqualTo("  foo   Foo Param\n  fizz  Fizz Param\n"));
    }

    [Test]
    public void OptionsString()
    {
        Assert.That(args.OptionsString(), Is.EqualTo(""));

        args.AddSwitchOption("a", "a-option", "Option A");
        Assert.That(args.OptionsString(), Is.EqualTo("  -a, --a-option  Option A\n"));

        args.AddValueOption("b", "b-option", "value", "Option B");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n";
        Assert.That(args.OptionsString(), Is.EqualTo(expected));

        args.AddMultiOption("c", "c-option", "value", "Option C");
        expected = "  -a, --a-option          Option A\n" +
                   "  -b, --b-option=<value>  Option B\n" +
                   "  -c, --c-option=<value>  Option C\n";
        Assert.That(args.OptionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void MultilineOptionsAreAlignedProperly()
    {
        args.AddSwitchOption("a", "a-option", "Option A");

        args.AddValueOption("b", "b-option", "value", "Option B\nmore info on b option");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n" +
                          "                          more info on b option\n";

        Assert.That(args.OptionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void LongOptionDescriptionsAreSplitIntoMultipleLines()
    {
        args.AddSwitchOption("a", "a-option", "Option A");

        args.AddValueOption("b", "b-option", "value", "Option B which has a really long description that should be cutoff at 72 chars.");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B which has a really long description that should be cutoff at 72\n" +
                          "                          chars.\n";

        Assert.That(args.OptionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void ExtraNewlinesArePreservedInOptionsString()
    {
        args.AddSwitchOption("a", "a-option", "Option A");

        args.AddValueOption("b", "b-option", "value", "Option B\n\nThat's it");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n" +
                          "                          \n" +
                          "                          That's it\n";

        Assert.That(args.OptionsString(), Is.EqualTo(expected));
    }

    private void CheckOptionError(string message, string shortName, string fullName, string description)
    {
        try
        {
            args.AddSwitchOption(shortName, fullName, description);
            Assert.Fail("should throw exception");
        }
        catch(SystemException e)
        {
            Assert.That(e.Message, Is.EqualTo(message));
        }
    }

    private void CheckParseError(string message, params string[] arguments)
    {
        results = args.Parse(arguments);
        IEnumerable<object> errors = (IEnumerable<object>)results["*errors"];
        Assert.That(errors, Is.Not.Null);
        String joinedErrors = "";

        foreach(Object error in errors)
            joinedErrors += (error.ToString() + ", ");

        Assert.That(errors.Contains(message), Is.True);
    }
  

    private IEnumerable<object> list(params object[] items) => items;

    private void AssertListsEquals(IEnumerable<object> expected, object actual)
    {
        if (!(actual is IEnumerable<object>))
            Assert.Fail("actual is not IEnumerable");

        var actualList = (IEnumerable<object>) actual;

        Assert.That(actualList.Count(), Is.EqualTo(expected.Count()));
        for (int i = 0; i < expected.Count(); i++)
            Assert.That(actualList.ElementAt(i), Is.EqualTo(expected.ElementAt(i)));
    }
}