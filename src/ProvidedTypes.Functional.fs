module ProviderImplementation.ProvidedTypes.Functional

open System
open System.Reflection
open FSharp.Quotations

type ProvidedParameter = ProviderImplementation.ProvidedTypes.ProvidedParameter
type ProvidedStaticParameter = ProviderImplementation.ProvidedTypes.ProvidedStaticParameter
type ProvidedConstructor = ProviderImplementation.ProvidedTypes.ProvidedConstructor
type ProvidedMethod = ProviderImplementation.ProvidedTypes.ProvidedMethod
type ProvidedProperty = ProviderImplementation.ProvidedTypes.ProvidedProperty
type ProvidedEvent = ProviderImplementation.ProvidedTypes.ProvidedEvent
type ProvidedField = ProviderImplementation.ProvidedTypes.ProvidedField
type ProvidedTypeBuilder = ProviderImplementation.ProvidedTypes.ProvidedTypeBuilder
type ProvidedMeasureBuilder = ProviderImplementation.ProvidedTypes.ProvidedMeasureBuilder
type ProvidedAssembly = ProviderImplementation.ProvidedTypes.ProvidedAssembly
type ProvidedTypeDefinition = ProviderImplementation.ProvidedTypes.ProvidedTypeDefinition

/// A base type providing default implementations of type provider functionality.
[<AbstractClass>]
type FunctionalProvider(config,
        build: Assembly -> list<ProvidedTypeDefinition>,
        ?sourceAssemblies: Assembly list,
        ?assemblyReplacementMap: (string * string) list,
        ?addDefaultProbingLocation: bool)
    as this =
    inherit TypeProviderForNamespaces(config,
        ?sourceAssemblies = sourceAssemblies,
        ?assemblyReplacementMap = assemblyReplacementMap,
        ?addDefaultProbingLocation = addDefaultProbingLocation)

    do
        Assembly.GetExecutingAssembly()
        |> build
        |> List.groupBy (fun ty -> ty.Namespace)
        |> List.iter this.AddNamespace

/// A set of static parameters for a type provider.
type ProvidedStaticParameters<'T> =
    { Parameters: ProvidedStaticParameter list
      Extract: obj list -> 'T * obj list }

module ProvidedStaticParameters =

    /// Create a mandatory static parameter with the given name.
    let mandatory<'T> name =
        { Parameters = [ ProvidedStaticParameter(name, typeof<'T>) ]
          Extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }

    /// Create an optional static parameter with the given name and default value.
    let optional<'T> name (defaultValue: 'T) =
        { Parameters = [ ProvidedStaticParameter(name, typeof<'T>, box defaultValue) ]
          Extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }
        
    /// Create an empty set of static parameters.
    let ret<'T> (x: 'T) =
        { Parameters = []
          Extract = fun args -> x, args }
        
    /// Map the value extracted from a set of static parameters.
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedStaticParameters<'T>) =
        { Parameters = p.Parameters
          Extract = fun args ->
            let x, rest = p.Extract args
            f x, rest }

    /// Combine two sets of static parameters.
    let map2<'T, 'U, 'V>
            (f: 'T -> 'U -> 'V)
            (p1: ProvidedStaticParameters<'T>)
            (p2: ProvidedStaticParameters<'U>)
            : ProvidedStaticParameters<'V> =
        { Parameters = p1.Parameters @ p2.Parameters
          Extract = fun args ->
            let x1, rest = p1.Extract args
            let x2, rest = p2.Extract rest
            f x1 x2, rest }

/// A set of parameters for a provided method.
type ProvidedParameters<'T> =
    { Parameters: ProvidedParameter list
      Extract: Expr list -> 'T * Expr list }

module ProvidedParameters =

    let mandatory<'T> name =
        { Parameters = [ ProvidedParameter(name, typeof<'T>) ]
          Extract = function
              | e :: rest -> Expr.Cast<'T> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let dynMandatory ty name =
        { Parameters = [ ProvidedParameter(name, ty) ]
          Extract = function
              | e :: rest -> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let optional<'T> name (defaultValue: 'T) =
        { Parameters = [ ProvidedParameter(name, typeof<'T>, optionalValue = box defaultValue) ]
          Extract = function
              | e :: rest -> Expr.Cast<'T> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let dynOptional ty name defaultValue =
        { Parameters = [ ProvidedParameter(name, ty, optionalValue = box defaultValue) ]
          Extract = function
              | e :: rest -> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let this<'T> =
        { Parameters = []
          Extract = function
              | e :: rest -> Expr.Cast<'T> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let dynThis =
        { Parameters = []
          Extract = function
              | e :: rest -> e, rest
              | args -> failwithf "Invalid args: %A" args }

    /// Create an empty set of static parameters.
    let ret<'T> (x: 'T) =
        { Parameters = []
          Extract = fun args -> x, args }

    /// Map the value extracted from a set of static parameters.
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedParameters<'T>) =
        { Parameters = p.Parameters
          Extract = fun args ->
            let x, rest = p.Extract args
            f x, rest }

    /// Combine two sets of static parameters.
    let map2<'T, 'U, 'V>
            (f: 'T -> 'U -> 'V)
            (p1: ProvidedParameters<'T>)
            (p2: ProvidedParameters<'U>)
            : ProvidedParameters<'V> =
        { Parameters = p1.Parameters @ p2.Parameters
          Extract = fun args ->
            let x1, rest = p1.Extract args
            let x2, rest = p2.Extract rest
            f x1 x2, rest }

    type ParametersBuilder() =
        member _.MergeSources(p1, p2) =
            map2 (fun x1 x2 -> (x1, x2)) p1 p2

        member _.BindReturn(p, f) =
            map f p

        member _.Return(x) =
            ret x

    /// Computation expression that creates a method body by binding its arguments with let!...and!.
    let bind = ParametersBuilder()

module ProvidedTypeDefinition =

    type AddStaticParametersBuilder() =
        member _.MergeSources(p1, p2) =
            ProvidedStaticParameters.map2 (fun x1 x2 -> (x1, x2)) p1 p2

        member _.BindReturn(p, f) =
            ProvidedStaticParameters.map f p

        member _.Return(x) =
            ProvidedStaticParameters.ret x

        member _.Run(p: ProvidedStaticParameters<string -> ProvidedTypeDefinition>) =
            fun (ty: ProvidedTypeDefinition) ->
                ty.DefineStaticParameters(p.Parameters, fun tyName args ->
                    let build, _ = p.Extract (List.ofArray args)
                    build tyName)
                ty

        member _.Run(p: ProvidedStaticParameters<ProvidedTypeDefinition -> ProvidedTypeDefinition>) =
            fun (ty: ProvidedTypeDefinition) ->
                ty.DefineStaticParameters(p.Parameters, fun tyName args ->
                    let ty = ProvidedTypeDefinition(ty.Assembly, ty.Namespace, tyName, None)
                    let build, _ = p.Extract (List.ofArray args)
                    build ty)
                ty

    /// Computation expression that adds static parameters to a ProvidedTypeDefinition by binding them with let!...and!.
    let addStaticParameters = AddStaticParametersBuilder()

    /// Add a set of members to a ProvidedTypeDefinition.
    let addMembers (items: list<#MemberInfo>) (ty: ProvidedTypeDefinition) =
        ty.AddMembers items
        ty

    /// Add a method, property, nested type or other member to a ProvidedTypeDefinition.
    let addMember (items: MemberInfo) (ty: ProvidedTypeDefinition) =
        ty.AddMember items
        ty

    /// Add a set of members to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    let addMembersDelayed (items: unit -> list<#MemberInfo>) (ty: ProvidedTypeDefinition) =
        ty.AddMembersDelayed items
        ty

    /// Add a member to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    let addMemberDelayed (items: unit -> #MemberInfo) (ty: ProvidedTypeDefinition) =
        ty.AddMemberDelayed items
        ty

module ProvidedField =

    /// Create a new provided literal field. It is not initially associated with any specific provided type definition.
    let literal<'T> (name: string) (value: 'T) =
        ProvidedField.Literal(name, typeof<'T>, box value)

module ProvidedMethod =

    type MethodBuilder(name: string, returnType: Type, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr>) =
            ProvidedMethod(name, p.Parameters, returnType, isStatic = isStatic,
                           invokeCode = fun args -> p.Extract args |> fst)

    type MethodBuilder<'ReturnType>(name: string, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<'ReturnType>>) =
            ProvidedMethod(name, p.Parameters, typeof<'ReturnType>, isStatic = isStatic,
                           invokeCode = fun args -> p.Extract args |> fst :> Expr)

    type ConstructorBuilder() =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<unit>>) =
            ProvidedConstructor(p.Parameters, fun args -> p.Extract args |> fst :> Expr)

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    let dynInstanceMethod returnType name = MethodBuilder(name, returnType, false)

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    let instanceMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, false)

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    let dynStaticMethod returnType name = MethodBuilder(name, returnType, true)

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    let staticMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, true)

    /// Computation expression that creates a constructor by binding its arguments with let!...and!.
    let constructor = ConstructorBuilder()
