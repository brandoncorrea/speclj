namespace speclj.platform;

public class SpecFailure : Exception
{
    public SpecFailure(String s) : base(s) { }
    public SpecFailure(String s, Exception cause) : base (s, cause) { }
}