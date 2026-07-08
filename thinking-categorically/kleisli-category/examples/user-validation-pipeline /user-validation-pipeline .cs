using System;

// Either monad implementation
public interface IEither<L, R>
{
    IEither<L, B> FlatMap<B>(Func<R, IEither<L, B>> f);
    IEither<L, B> Map<B>(Func<R, B> f);
}

public class Left<L, R> : IEither<L, R>
{
    private readonly L value;
    public Left(L value) { this.value = value; }

    public IEither<L, B> FlatMap<B>(Func<R, IEither<L, B>> f)
    {
        return new Left<L, B>(value);
    }

    public IEither<L, B> Map<B>(Func<R, B> f)
    {
        return new Left<L, B>(value);
    }

    public override string ToString() => $"Left({value})";
}

public class Right<L, R> : IEither<L, R>
{
    private readonly R value;
    public Right(R value) { this.value = value; }

    public IEither<L, B> FlatMap<B>(Func<R, IEither<L, B>> f)
    {
        return f(value);
    }

    public IEither<L, B> Map<B>(Func<R, B> f)
    {
        return new Right<L, B>(f(value));
    }

    public override string ToString() => $"Right({value})";
}

// Domain types
public class User
{
    public string Name { get; }
    public int Age { get; }
    public string Email { get; }

    public User(string name, int age, string email)
    {
        Name = name;
        Age = age;
        Email = email;
    }

    public override string ToString() => $"User({Name}, {Age}, {Email})";
}

public enum ValidationError { EmptyName, InvalidAge, InvalidEmail }

public static class UserValidationExample
{
    // Kleisli arrows for validation
    public static IEither<ValidationError, string> ValidateName(string name)
    {
        return string.IsNullOrEmpty(name)
            ? new Left<ValidationError, string>(ValidationError.EmptyName)
            : new Right<ValidationError, string>(name);
    }

    public static IEither<ValidationError, int> ValidateAge(string ageStr)
    {
        if (!int.TryParse(ageStr, out int age) || age < 0 || age > 150)
            return new Left<ValidationError, int>(ValidationError.InvalidAge);
        return new Right<ValidationError, int>(age);
    }

    public static IEither<ValidationError, string> ValidateEmail(string email)
    {
        return email.Contains("@")
            ? new Right<ValidationError, string>(email)
            : new Left<ValidationError, string>(ValidationError.InvalidEmail);
    }

    // Kleisli composition for user creation
    public static IEither<ValidationError, User> CreateUser(string name, string ageStr, string email)
    {
        return ValidateName(name)
            .FlatMap(validName =>
                ValidateAge(ageStr)
                    .FlatMap(validAge =>
                        ValidateEmail(email)
                            .Map(validEmail => new User(validName, validAge, validEmail))
                    )
            );
    }

    public static void Main()
    {
        Console.WriteLine(CreateUser("Alice", "25", "alice@example.com")); // Right(User)
        Console.WriteLine(CreateUser("", "25", "alice@example.com"));     // Left(EmptyName)
    }
}
