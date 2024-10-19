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
        results = args.parse();
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
        args.addParameter("foo", "Some Description");

        results = args.parse("bar");
        Assert.That(results["foo"], Is.EqualTo("bar"));

        results = args.parse("fizz");
        Assert.That(results["foo"], Is.EqualTo("fizz"));
    }

    [Test]
    public void MissingParameter()
    {
        args.addParameter("foo", "Some Description");
        CheckParseError("Missing parameter: foo");
    }

    [Test]
    public void ParsingTwoParameters()
    {
        args.addParameter("foo", "Some Description");
        args.addParameter("bar", "Some Description");

        results = args.parse("fizz", "bang");

        Assert.That(results["foo"], Is.EqualTo("fizz"));
        Assert.That(results["bar"], Is.EqualTo("bang"));
    }

    [Test]
    public void MissingOneOfTwoParameters()
    {
        args.addParameter("foo", "Some Description");
        args.addParameter("bar", "Some Description");

        CheckParseError("Missing parameter: foo");
        CheckParseError("Missing parameter: bar", "fizz");
    }

    [Test]
    public void OptionalParameter()
    {
        args.addOptionalParameter("foo", "Some Description");

        results = args.parse();
        Assert.That(results.ContainsKey("foo"), Is.False);
        Assert.That(results.ContainsKey("*errors"), Is.False);

        results = args.parse("fizz");
        Assert.That(results["foo"], Is.EqualTo("fizz"));
        Assert.That(results.ContainsKey("*errors"), Is.False);
    }

    [Test]
    public void OneSwitchOption()
    {
        args.addSwitchOption("m", "my-option", "my test option");

        results = args.parse();
        Assert.That(results.ContainsKey("my-option"), Is.False);

        results = args.parse("-m");
        Assert.That(results["my-option"], Is.EqualTo("on"));

        results = args.parse("--my-option");
        Assert.That(results["my-option"], Is.EqualTo("on"));
    }

    [Test]
    public void TwoSwitchOptions()
    {
        args.addSwitchOption("a", "a-option", "Option A");
        args.addSwitchOption("b", "b-option", "Option B");

        results = args.parse();
        Assert.That(results.ContainsKey("a-option"), Is.False);
        Assert.That(results.ContainsKey("b-option"), Is.False);

        results = args.parse("-a");
        Assert.That(results.ContainsKey("a-option"), Is.True);
        Assert.That(results.ContainsKey("b-option"), Is.False);

        results = args.parse("--b-option");
        Assert.That(results.ContainsKey("a-option"), Is.False);
        Assert.That(results.ContainsKey("b-option"), Is.True);

        results = args.parse("--a-option", "-b");
        Assert.That(results.ContainsKey("a-option"), Is.True);
        Assert.That(results.ContainsKey("b-option"), Is.True);
    }
    
    [Test]
    public void OptionNamesAreRequired()
    {
        CheckOptionError("Options require a shortName and fullName", "a", null, null);
        CheckOptionError("Options require a shortName and fullName", null, "a-option", null);
        args.addSwitchOption("a", "a-option", null);
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
        args.addValueOption("a", "a-option", "value", "Option A");

        results = args.parse("-a", "value");
        Assert.That(results["a-option"], Is.EqualTo("value"));

        results = args.parse("--a-option=value");
        Assert.That(results["a-option"], Is.EqualTo("value"));
    }

    [Test]
    public void MissingOptionValue()
    {
        args.addValueOption("a", "a-option", "value", "Option A");

        CheckParseError("Missing value for option: a", "-a");
        CheckParseError("Missing value for option: a-option", "--a-option");
    }

    [Test]
    public void MissingOptionValueWhenFollowedByOption()
    {
        args.addValueOption("a", "a-option", "value", "Option A");
        args.addSwitchOption("b", "b-option", "Option B");

        CheckParseError("Missing value for option: a", "-a", "-b");
        CheckParseError("Missing value for option: a", "-a", "--b-option");
    }

    [Test]
    public void ParameterWithSwitchOption()
    {
        args.addParameter("param", "Some Description");
        args.addSwitchOption("a", "a-option", "Option A");

        CheckParseError("Missing parameter: param");
        CheckParseError("Missing parameter: param", "-a");
        CheckParseError("Missing parameter: param", "--a-option");

        results = args.parse("-a", "blah");
        Assert.That(results["a-option"], Is.EqualTo("on"));
        Assert.That(results["param"], Is.EqualTo("blah"));

        results = args.parse("--a-option", "blah");
        Assert.That(results["a-option"], Is.EqualTo("on"));
        Assert.That(results["param"], Is.EqualTo("blah"));
    }

    [Test]
    public void ParameterWithValueOption()
    {
        args.addParameter("param", "Some Description");
        args.addValueOption("a", "a-option", "value", "Option A");

        CheckParseError("Missing parameter: param");
        CheckParseError("Missing value for option: a", "-a");
        CheckParseError("Missing parameter: param", "-a", "foo");
        CheckParseError("Missing parameter: param", "--a-option=foo");

        results = args.parse("-a", "foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));

        results = args.parse("--a-option=foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));
    }

    [Test]
    public void ParameterOptionsAreParsableInLongFormWithoutEqualsSign()
    {
        args.addParameter("param", "Some Description");
        args.addValueOption("a", "a-option", "value", "Option A");

        results = args.parse("--a-option", "foo", "bar");
        Assert.That(results["a-option"], Is.EqualTo("foo"));
        Assert.That(results["param"], Is.EqualTo("bar"));
    }

    [Test]
    public void RemainingArgs()
    {
        results = args.parse("foo");
        AssertListsEquals(list("foo"), results["*leftover"]);

        args.addParameter("param", "Some Description");
        results = args.parse("foo", "bar");
        AssertListsEquals(list("bar"), results["*leftover"]);

        args.addSwitchOption("a", "a-option", "Option A");
        results = args.parse("-a", "foo", "bar");
        AssertListsEquals(list("bar"), results["*leftover"]);

        results = args.parse("-z", "foo", "bar");
        AssertListsEquals(list("-z", "bar"), results["*leftover"]);
    }

    [Test]
    public void RemainingArgsWithValueOption()
    {
        args.addParameter("param", "Some Description");
        args.addValueOption("a", "a-option", "value", "Option A");

        results = args.parse("-z");
        AssertListsEquals(list("-z"), results["*leftover"]);

        results = args.parse("-z", "foo", "bar");
        AssertListsEquals(list("-z", "bar"), results["*leftover"]);

        results = args.parse("-a", "foo", "bar", "fizz");
        AssertListsEquals(list("fizz"), results["*leftover"]);
    }

    [Test]
    public void CanParseOptionsMixedInWithParameters()
    {
        args.addParameter("param1", "Some Description");
        args.addParameter("param2", "Some Description");
        args.addSwitchOption("a", "a-switch", "Switch A");
        args.addValueOption("b", "b-option", "B", "Option B");
        args.addValueOption("c", "c-option", "C", "Option C");

        results = args.parse("-a", "one", "--b-option=two", "three", "--c-option", "four", "five");

        Assert.That(results["a-switch"], Is.EqualTo("on"));
        Assert.That(results["param1"], Is.EqualTo("one"));
        Assert.That(results["b-option"], Is.EqualTo("two"));
        Assert.That(results["param2"], Is.EqualTo("three"));
        Assert.That(results["c-option"], Is.EqualTo("four"));
    }

    [Test]
    public void MultiParameters()
    {
        args.addMultiParameter("colors", "Any number of colors");

        results = args.parse("red", "orange", "yellow");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red", "orange", "yellow"), results["colors"]);

        results = args.parse();
        Assert.That(results.ContainsKey("*errors"), Is.False);
        Assert.That(results.ContainsKey("colors"), Is.False);

        results = args.parse("red");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red"), results["colors"]);
    }

    [Test]
    public void multiOptions()
    {
        args.addMultiOption("c", "color", "COLOR", "Some colors");

        results = args.parse();
        Assert.That(results.ContainsKey("*errors"), Is.False);
        Assert.That(results.ContainsKey("color"), Is.False);

        results = args.parse("-c", "red");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red"), results["color"]);

        results = args.parse("-c", "red", "--color", "orange", "--color=yellow");
        Assert.That(results.ContainsKey("*errors"), Is.False);
        AssertListsEquals(list("red", "orange", "yellow"), results["color"]);
    }

    [Test]
    public void ArgString()
    {
        Assert.That(args.argString(), Is.EqualTo(""));

        args.addParameter("param", "Some Description");
        Assert.That(args.argString(), Is.EqualTo("<param>"));

        args.addSwitchOption("a", "a-option", "Option A");
        Assert.That(args.argString(), Is.EqualTo("[options] <param>"));

        args.addParameter("another-param", "Some Description");
        Assert.That(args.argString(), Is.EqualTo("[options] <param> <another-param>"));

        args.addOptionalParameter("param3", "Parameter 3");
        Assert.That(args.argString(), Is.EqualTo("[options] <param> <another-param> [param3]"));

        args.addMultiParameter("param4", "Parameter 4");
        Assert.That(args.argString(), Is.EqualTo("[options] <param> <another-param> [param3] [param4*]"));
    }

    [Test]
    public void ArgStringWithOptionalParameter()
    {
        args.addOptionalParameter("param", "Some Description");
        Assert.That(args.argString(), Is.EqualTo("[param]"));
    }

    [Test]
    public void ParametersString()
    {
        Assert.That(args.parametersString(), Is.EqualTo(""));

        args.addParameter("foo", "Foo Param");
        Assert.That(args.parametersString(), Is.EqualTo("  foo  Foo Param\n"));

        args.addParameter("fizz", "Fizz Param");
        Assert.That(args.parametersString(), Is.EqualTo("  foo   Foo Param\n  fizz  Fizz Param\n"));
    }

    [Test]
    public void OptionsString()
    {
        Assert.That(args.optionsString(), Is.EqualTo(""));

        args.addSwitchOption("a", "a-option", "Option A");
        Assert.That(args.optionsString(), Is.EqualTo("  -a, --a-option  Option A\n"));

        args.addValueOption("b", "b-option", "value", "Option B");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n";
        Assert.That(args.optionsString(), Is.EqualTo(expected));

        args.addMultiOption("c", "c-option", "value", "Option C");
        expected = "  -a, --a-option          Option A\n" +
                   "  -b, --b-option=<value>  Option B\n" +
                   "  -c, --c-option=<value>  Option C\n";
        Assert.That(args.optionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void MultilineOptionsAreAlignedProperly()
    {
        args.addSwitchOption("a", "a-option", "Option A");

        args.addValueOption("b", "b-option", "value", "Option B\nmore info on b option");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n" +
                          "                          more info on b option\n";

        Assert.That(args.optionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void LongOptionDescriptionsAreSplitIntoMultipleLines()
    {
        args.addSwitchOption("a", "a-option", "Option A");

        args.addValueOption("b", "b-option", "value", "Option B which has a really long description that should be cutoff at 72 chars.");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B which has a really long description that should be cutoff at 72\n" +
                          "                          chars.\n";

        Assert.That(args.optionsString(), Is.EqualTo(expected));
    }

    [Test]
    public void ExtraNewlinesArePreservedInOptionsString()
    {
        args.addSwitchOption("a", "a-option", "Option A");

        args.addValueOption("b", "b-option", "value", "Option B\n\nThat's it");
        string expected = "  -a, --a-option          Option A\n" +
                          "  -b, --b-option=<value>  Option B\n" +
                          "                          \n" +
                          "                          That's it\n";

        Assert.That(args.optionsString(), Is.EqualTo(expected));
    }

    private void CheckOptionError(string message, string shortName, string fullName, string description)
    {
        try
        {
            args.addSwitchOption(shortName, fullName, description);
            Assert.Fail("should throw exception");
        }
        catch(SystemException e)
        {
            Assert.That(e.Message, Is.EqualTo(message));
        }
    }

    private void CheckParseError(string message, params string[] arguments)
    {
        results = args.parse(arguments);
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