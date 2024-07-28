using System.Collections;
using System.Diagnostics.CodeAnalysis;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using Mono.Cecil;
using Mono.Cecil.Rocks;

namespace PackageDump;

public partial class Program
{
    static readonly Dictionary<string, string> TypeMappings = new()
    {
        // Intrinsics
        { typeof(void).FullName!, "void" },
        { typeof(bool).FullName!, "bool" },
        { typeof(byte).FullName!, "byte" },
        { typeof(sbyte).FullName!, "sbyte" },
        { typeof(char).FullName!, "char" },
        { typeof(decimal).FullName!, "decimal" },
        { typeof(double).FullName!, "double" },
        { typeof(float).FullName!, "float" },
        { typeof(int).FullName!, "int" },
        { typeof(uint).FullName!, "uint" },
        { typeof(long).FullName!, "long" },
        { typeof(ulong).FullName!, "ulong" },
        { typeof(object).FullName!, "object" },
        { typeof(short).FullName!, "short" },
        { typeof(ushort).FullName!, "ushort" },
        { typeof(string).FullName!, "string" },
        { typeof(IntPtr).FullName!, "nint" },
        { typeof(UIntPtr).FullName!, "nuint" },

        // Commonly used types
        { typeof(Exception).FullName!, "Exception" },
        { typeof(IEnumerable).FullName!, "IEnumerable" },
        { typeof(IEnumerator).FullName!, "IEnumerable" },
        { typeof(Task).FullName!, "Task" },
        { typeof(Type).FullName!, "Type" },
        { typeof(ValueTask).FullName!, "ValueTask" },

        // Generics which get their tick removed, that we want to simplify
        { "System.Collections.Generic.IEnumerable", "IEnumerable" },
        { "System.Collections.Generic.IEnumerator", "IEnumerator" },
        { "System.Collections.Generic.IReadOnlyCollection", "IReadOnlyCollection" },
        { "System.Collections.Generic.KeyValuePair", "KeyValuePair" },
    };

    public static int Main(string[] args)
    {
        try
        {
            var packageCachePath = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
            if (packageCachePath is null)
            {
                var homeFolder =
                    Environment.GetEnvironmentVariable("USERPROFILE")
                        ?? Environment.GetEnvironmentVariable("HOME")
                        ?? throw new InvalidOperationException("Cannot find user's home folder");
                packageCachePath = Path.Combine(homeFolder, ".nuget", "packages");
            }

            if (!Directory.Exists(packageCachePath))
                throw new InvalidOperationException($"Package cache folder '{packageCachePath}' does not exist");

            if (args.Length < 2)
            {
                Console.WriteLine("usage: <output_folder> <package_name@version> [package_name@version...]");
                return 2;
            }

            var types = new Dictionary<string, List<(TypeDefinition Type, List<(string File, string TargetFramework)> Assemblies)>>();
            var outputFolder = Path.GetFullPath(args[0]);
            if (Directory.Exists(outputFolder))
                Directory.Delete(outputFolder, true);
            Directory.CreateDirectory(outputFolder);

            Console.WriteLine("Output: {0}", outputFolder);

            foreach (var arg in args.Skip(1))
            {
                var versionIdx = arg.IndexOf('@');
                if (versionIdx == -1)
                    throw new InvalidOperationException($"Argument '{arg}' is malformed: must be 'packagename@version'");

                var packageName = arg[..versionIdx];
                var version = arg[(versionIdx + 1)..];
                var packageBasePath = Path.Combine(packageCachePath, packageName.ToLowerInvariant(), version);
                if (!Directory.Exists(packageBasePath))
                    throw new InvalidOperationException($"Package folder '{packageBasePath}' does not exist");

                var nuspecFile = Path.Combine(packageBasePath, $"{packageName}.nuspec");
                if (!File.Exists(nuspecFile))
                    throw new InvalidOperationException($"Package nuspec file '{nuspecFile}' does not exist");

                var references = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                using (var nuspecStream = File.OpenRead(nuspecFile))
                {
                    var nuspecXmlRoot = XDocument.Load(nuspecStream);
                    var defaultNamespace = nuspecXmlRoot.Root?.GetDefaultNamespace().NamespaceName;
                    XElement[] referenceNodes;

                    if (!string.IsNullOrEmpty(defaultNamespace))
                    {
                        var resolver = new XmlNamespaceManager(new NameTable());
                        resolver.AddNamespace("x", defaultNamespace);
                        referenceNodes = nuspecXmlRoot.XPathSelectElements("/x:package/x:metadata/x:references/x:reference", resolver).ToArray();
                    }
                    else
                        referenceNodes = nuspecXmlRoot.XPathSelectElements("/package/metadata/references/reference").ToArray();

                    foreach (var referenceNode in referenceNodes)
                        if (referenceNode.Attribute("file")?.Value.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) == true)
                            references.Add(referenceNode.Attribute("file")!.Value);
                }

                var libFolder = Path.Combine(packageBasePath, "lib");

                foreach (var dllFile in Directory.GetFiles(libFolder, "*.dll", SearchOption.AllDirectories).OrderBy(f => f))
                    if (references.Count == 0 || references.Contains(Path.GetFileName(dllFile)))
                    {
                        using var moduleDefinition = ModuleDefinition.ReadModule(dllFile);
                        var targetFramework = Path.GetFileName(Path.GetDirectoryName(dllFile))!;
                        var baseFileName = Path.GetFileName(dllFile);

                        void processType(TypeDefinition type)
                        {
                            if (!types.TryGetValue(type.Name, out var list))
                            {
                                list = [];
                                types[type.Name] = list;
                            }

                            var added = false;

                            foreach (var other in list)
                                if (TypeComparer.Instance.Equals(type, other.Type))
                                {
                                    added = true;
                                    other.Assemblies.Add((baseFileName, targetFramework));
                                }

                            if (!added)
                            {
                                if (list.Count != 0)
                                    Console.WriteLine("  Found alternate type definition: {0}", type.Name);

                                list.Add((type, [(baseFileName, targetFramework)]));
                            }

                            foreach (var nestedType in type.NestedTypes.Where(t => t.IsPublic || t.IsNestedPublic))
                                processType(nestedType);
                        }

                        foreach (var type in moduleDefinition.Types.Where(t => t.IsPublic || t.IsNestedPublic))
                            processType(type);
                    }
            }

            foreach (var typeByName in types.OrderBy(kvp => kvp.Key))
            {
                var typeFileName = Path.Join(outputFolder, $"{typeByName.Key}.txt");
                using var typeFile = File.OpenWrite(typeFileName);
                using var typeFileWriter = new StreamWriter(typeFile);

                foreach (var typeInfo in typeByName.Value.OrderBy(kvp => kvp.Type.Namespace))
                {
                    typeFileWriter.WriteLine(typeInfo.Type.Name);

                    typeFileWriter.WriteLine("  declaration");
                    typeFileWriter.WriteLine(
                        "    {0}{1} {2} {3}",
                        GetAttributes(typeInfo.Type),
                        GetTypeType(typeInfo.Type),
                        GetTypeName(typeInfo.Type),
                        GetTypeBase(typeInfo.Type)
                    );
                    var attributeUsageAttribute = typeInfo.Type.CustomAttributes.FirstOrDefault(a => a.AttributeType.FullName == typeof(AttributeUsageAttribute).FullName);
                    if (attributeUsageAttribute is not null)
                        typeFileWriter.WriteLine("    targets: {0}", (AttributeTargets)attributeUsageAttribute.ConstructorArguments[0].Value);

                    if (typeByName.Value.Count > 1)
                    {
                        typeFileWriter.WriteLine("  files");
                        foreach (var file in typeInfo.Assemblies.OrderBy(a => a.File).ThenBy(a => a.TargetFramework))
                            typeFileWriter.WriteLine("    {0} ({1})", file.File, file.TargetFramework);
                    }

                    typeFileWriter.WriteLine("  constructors");
                    foreach (var ctor in GetConstructors(typeInfo.Type))
                        typeFileWriter.WriteLine("    {0}", GetMethodDefinition(ctor, typeInfo.Type.Name));

                    if (typeInfo.Type.IsEnum)
                        typeFileWriter.WriteLine("  {0}", typeInfo.Type.CustomAttributes.Any(a => a.AttributeType.FullName == typeof(FlagsAttribute).FullName) ? "flags" : "values");
                    else
                        typeFileWriter.WriteLine("  fields");

                    foreach (var field in GetFields(typeInfo.Type))
                        typeFileWriter.WriteLine("    {0}{1}", field.Name, field.Constant is not null ? $" = {field.Constant}" : "");

                    typeFileWriter.WriteLine("  properties");
                    foreach (var property in GetProperties(typeInfo.Type))
                        typeFileWriter.WriteLine(
                            "    {0} {1} {{ {2}{3}}}",
                            GetTypeName(property.PropertyType),
                            property.Name,
                            GetPropertyAccessor(property.GetMethod, "get"),
                            GetPropertyAccessor(property.SetMethod, "set")
                        );

                    typeFileWriter.WriteLine("  methods");
                    foreach (var method in GetMethods(typeInfo.Type))
                        typeFileWriter.WriteLine("    {0}", GetMethodDefinition(method));

                    typeFileWriter.WriteLine();
                }
            }
        }
        catch (Exception ex)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine(ex.Message);
            Console.ForegroundColor = ConsoleColor.DarkGray;
            Console.WriteLine(ex.StackTrace);
            Console.ResetColor();
            return -1;
        }

        return 0;
    }

    static string GetAttributes(MethodDefinition method)
    {
        var result = string.Empty;
        result += (method.Attributes & MethodAttributes.MemberAccessMask) switch
        {
            MethodAttributes.Public => "public ",
            MethodAttributes.Family => "protected ",
            MethodAttributes.Private => "private ",
            MethodAttributes.Assembly => "internal ",
            MethodAttributes.FamORAssem => "protected internal ",
            _ => $"???{method.Attributes & MethodAttributes.MemberAccessMask} ",
        };
        if ((method.Attributes & MethodAttributes.Static) == MethodAttributes.Static)
            result += "static ";
        if ((method.Attributes & MethodAttributes.Final) == MethodAttributes.Final)
            result += "sealed ";
        return result;
    }

    static string GetAttributes(TypeDefinition type)
    {
        var result = (type.Attributes & TypeAttributes.VisibilityMask) switch
        {
            TypeAttributes.Public => "public ",
            TypeAttributes.NestedPublic => "nested public ",
            _ => $"???{type.Attributes & TypeAttributes.VisibilityMask} ",
        };

        if ((type.Attributes & TypeAttributes.Abstract) == TypeAttributes.Abstract)
            result += "abstract ";
        if ((type.Attributes & TypeAttributes.Sealed) == TypeAttributes.Sealed)
            result += "sealed ";

        return result;
    }

    internal static MethodDefinition[] GetConstructors(TypeDefinition type)
    {
        var result = type.GetConstructors().Where(c => !c.IsPrivate).ToArray();

        if (result.Length == 1 && result[0].Parameters.Count == 0)
            return [];

        return result;
    }

    internal static FieldDefinition[] GetFields(TypeDefinition type) =>
        [.. type.Fields.Where(f => !f.IsSpecialName && f.IsPublic).OrderBy(f => f.Name)];

    static string GetMethodDefinition(
        MethodDefinition method,
        string? name = null) =>
            string.Format(
                "{0}{1}{2}{3}({4})",
                method.CustomAttributes.Any(a => a.AttributeType.FullName == typeof(ObsoleteAttribute).FullName) ? "[Obsolete] " : "",
                GetAttributes(method),
                name is null ? $"{GetTypeName(method.ReturnType)} " : "",
                name ?? method.Name,
                string.Join(", ", method.Parameters.Select(GetParameterName))
            );

    // TODO: How to order overloads?
    internal static MethodDefinition[] GetMethods(TypeDefinition type) =>
        [.. type.Methods.Where(m => (m.IsPublic || m.IsFamily || m.IsFamilyOrAssembly) && !m.IsSpecialName).OrderBy(m => m.Name)];

    static string GetParameterName(ParameterDefinition parameter) =>
        $"{(parameter.CustomAttributes.Any(a => a.AttributeType.FullName == typeof(ParamArrayAttribute).FullName) ? "params " : "")}{GetTypeName(parameter.ParameterType)} {parameter.Name}";

    internal static PropertyDefinition[] GetProperties(TypeDefinition type) =>
        [.. type.Properties.OrderBy(p => p.Name)];

    static string GetPropertyAccessor(
        MethodDefinition? method,
        string name)
    {
        if (method is null)
            return string.Empty;

        return $"{GetAttributes(method)}{name}; ";
    }

    static string GetTypeBase(TypeDefinition type)
    {
        var bases = new List<string>();

        if (type.BaseType is not null && type.BaseType.FullName != typeof(object).FullName)
            bases.Add(GetTypeName(type.BaseType));

        foreach (var @interface in type.Interfaces)
            bases.Add(GetTypeName(@interface.InterfaceType));

        if (bases.Count == 0)
            return string.Empty;

        return $": {string.Join(", ", bases)} ";
    }

    static readonly Regex genericRegex = GetGenericRegex();

    static string GetTypeName(TypeReference type)
    {
        if (type.GenericParameters.Count == 0)
            return GetTypeName(type.FullName);

        var tickIndex = type.FullName.IndexOf('`');
        var fullName = tickIndex < 0 ? type.FullName : type.FullName[..tickIndex];
        return $"{GetTypeName(fullName)}<{string.Join(", ", type.GenericParameters.Select(p => p.Name))}>";

        static string GetTypeName(
            string typeName,
            int splitCount = 0)
        {
            if (typeName.StartsWith("System.Nullable`1<"))
                return GetTypeName(typeName[18..^1]) + "?";

            var match = genericRegex.Match(typeName);
            if (match.Success)
                return $"{GetTypeName(match.Groups[1].Value)}<{GetTypeName(match.Groups[3].Value, int.Parse(match.Groups[2].Value))}>";

            if (typeName.EndsWith("[]"))
                return GetTypeName(typeName[..^2]) + "[]";

            if (splitCount > 1)
                return string.Join(", ", typeName.Split(',').Select(part => GetTypeName(part)));

            if (TypeMappings.TryGetValue(typeName, out var shortName))
                return shortName;

            return typeName;
        }
    }

    static string GetTypeType(TypeDefinition type) =>
        type.IsEnum ? "enum" : (type.IsValueType ? "struct" : (type.IsInterface ? "interface" : "class"));

    [GeneratedRegex("^(.*?)`(\\d+)<(.*?)>$", RegexOptions.Compiled)]
    private static partial Regex GetGenericRegex();
}

public class TypeComparer : IEqualityComparer<TypeDefinition>
{
    public static TypeComparer Instance { get; } = new();

    public bool Equals(
        TypeDefinition? x,
        TypeDefinition? y)
    {
        if (x is null)
            return y is null;
        if (y is null)
            return false;

        if (x.Name != y.Name || x.Namespace != y.Namespace)
            return false;

        return
            GetBaseType(x)?.FullName == GetBaseType(y)?.FullName &&
            x.Interfaces.Count == y.Interfaces.Count &&
            Program.GetConstructors(x).Length == Program.GetConstructors(y).Length &&
            Program.GetFields(x).Length == Program.GetFields(y).Length &&
            Program.GetMethods(x).Length == Program.GetMethods(y).Length &&
            Program.GetProperties(x).Length == Program.GetProperties(y).Length;
    }

    static TypeReference? GetBaseType(TypeDefinition type)
    {
        if (type.BaseType is null)
            return null;
        if (type.BaseType.FullName == typeof(object).FullName)
            return null;
        return type.BaseType;
    }

    public int GetHashCode([DisallowNull] TypeDefinition obj)
    {
        var hashCode = new HashCode();

        hashCode.Add(obj.Name);
        hashCode.Add(obj.Namespace);

        hashCode.Add(GetBaseType(obj)?.FullName);
        hashCode.Add(obj.Interfaces.Count);
        hashCode.Add(Program.GetConstructors(obj).Length);
        hashCode.Add(Program.GetFields(obj).Length);
        hashCode.Add(Program.GetMethods(obj).Length);
        hashCode.Add(Program.GetProperties(obj).Length);

        return hashCode.ToHashCode();
    }
}
