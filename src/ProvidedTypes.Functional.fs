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
    { parameters: ProvidedStaticParameter list
      extract: obj list -> 'T * obj list }

module ProvidedStaticParameters =

    /// Create a mandatory static parameter with the given name.
    let mandatory<'T> name =
        { parameters = [ ProvidedStaticParameter(name, typeof<'T>) ]
          extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }

    /// Create an optional static parameter with the given name and default value.
    let optional<'T> name (defaultValue: 'T) =
        { parameters = [ ProvidedStaticParameter(name, typeof<'T>, box defaultValue) ]
          extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }

    /// Create an empty set of static parameters.
    let ret<'T> (x: 'T) =
        { parameters = []
          extract = fun args -> x, args }

    /// Map the value extracted from a set of static parameters.
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedStaticParameters<'T>) =
        { parameters = p.parameters
          extract = fun args ->
            let x, rest = p.extract args
            f x, rest }

    /// Combine two sets of static parameters.
    let map2<'T, 'U, 'V>
            (f: 'T -> 'U -> 'V)
            (p1: ProvidedStaticParameters<'T>)
            (p2: ProvidedStaticParameters<'U>)
            : ProvidedStaticParameters<'V> =
        { parameters = p1.parameters @ p2.parameters
          extract = fun args ->
            let x1, rest = p1.extract args
            let x2, rest = p2.extract rest
            f x1 x2, rest }

/// Description of a provided method parameter.
type ProvidedParameterDescription<'T> =
    { name: string
      isOut: bool
      defaultValue: 'T option
      isParamArray: bool
      isReflectedDefinition: bool
      customAttributes: CustomAttributeData list }

/// Description of a provided method parameter.
type ProvidedParameterDescription =
    { name: string
      parameterType: Type
      isOut: bool
      defaultValue: obj option
      isParamArray: bool
      isReflectedDefinition: bool
      customAttributes: CustomAttributeData list }

/// A set of parameters for a provided method.
type ProvidedParameters<'T> =
    { parameters: ProvidedParameter list
      extract: Expr list -> 'T * Expr list }

module ProvidedParameters =

    /// Create a mandatory parameter with the given name.
    let mandatory<'T> name : ProvidedParameterDescription<'T> =
        { name = name
          isOut = false
          defaultValue = None
          isParamArray = false
          isReflectedDefinition = false
          customAttributes = [] }

    /// Create a mandatory parameter with the given name.
    let dynMandatory ty name =
        { name = name
          parameterType = ty
          isOut = false
          defaultValue = None
          isParamArray = false
          isReflectedDefinition = false
          customAttributes = [] }

    /// Create an optional parameter with the given name and default value.
    let optional<'T> name (defaultValue: 'T) =
        { mandatory<'T> name with
            defaultValue = Some defaultValue }

    /// Create an optional parameter with the given name and default value.
    let dynOptional ty name defaultValue =
        { dynMandatory ty name with
            defaultValue = Some defaultValue }

    /// Create a "this" parameter for an instance method.
    let this<'T> =
        { parameters = []
          extract = function
              | e :: rest -> Expr.Cast<'T> e, rest
              | args -> failwithf "Invalid args: %A" args }

    /// Create a "this" parameter for an instance method.
    let dynThis =
        { parameters = []
          extract = function
              | e :: rest -> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let single (descr: ProvidedParameterDescription<'T>) =
        let p = ProvidedParameter(descr.name, typeof<'T>, descr.isOut,
            ?optionalValue = Option.map box descr.defaultValue,
            IsParamArray = descr.isParamArray,
            IsReflectedDefinition = descr.isReflectedDefinition)
        for a in descr.customAttributes do p.AddCustomAttribute(a)
        { parameters = [ p ]
          extract = function
              | e :: rest -> Expr.Cast<'T> e, rest
              | args -> failwithf "Invalid args: %A" args }

    let dynSingle descr =
        let p = ProvidedParameter(descr.name, descr.parameterType, descr.isOut,
            ?optionalValue = descr.defaultValue,
            IsParamArray = descr.isParamArray,
            IsReflectedDefinition = descr.isReflectedDefinition)
        for a in descr.customAttributes do p.AddCustomAttribute(a)
        { parameters = [ p ]
          extract = function
              | e :: rest -> e, rest
              | args -> failwithf "Invalid args: %A" args }

    /// Create an empty set of static parameters.
    let ret<'T> (x: 'T) =
        { parameters = []
          extract = fun args -> x, args }

    /// Map the value extracted from a set of static parameters.
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedParameters<'T>) =
        { parameters = p.parameters
          extract = fun args ->
            let x, rest = p.extract args
            f x, rest }

    /// Combine two sets of static parameters.
    let map2<'T, 'U, 'V>
            (f: 'T -> 'U -> 'V)
            (p1: ProvidedParameters<'T>)
            (p2: ProvidedParameters<'U>)
            : ProvidedParameters<'V> =
        { parameters = p1.parameters @ p2.parameters
          extract = fun args ->
            let x1, rest = p1.extract args
            let x2, rest = p2.extract rest
            f x1 x2, rest }

    type ParametersBuilder() =
        member _.Source(p: ProvidedParameterDescription<'T>) = single p
        member _.Source(p: ProvidedParameterDescription) = dynSingle p
        member _.Source(p: ProvidedParameters<'T>) = p

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
                ty.DefineStaticParameters(p.parameters, fun tyName args ->
                    let build, _ = p.extract (List.ofArray args)
                    build tyName)
                ty

        member _.Run(p: ProvidedStaticParameters<ProvidedTypeDefinition -> ProvidedTypeDefinition>) =
            fun (ty: ProvidedTypeDefinition) ->
                ty.DefineStaticParameters(p.parameters, fun tyName args ->
                    let ty = ProvidedTypeDefinition(ty.Assembly, ty.Namespace, tyName, None)
                    let build, _ = p.extract (List.ofArray args)
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
            ProvidedMethod(name, p.parameters, returnType, isStatic = isStatic,
                           invokeCode = fun args -> p.extract args |> fst)

    type MethodBuilder<'ReturnType>(name: string, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<'ReturnType>>) =
            ProvidedMethod(name, p.parameters, typeof<'ReturnType>, isStatic = isStatic,
                           invokeCode = fun args -> p.extract args |> fst :> Expr)

    type ConstructorBuilder() =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<unit>>) =
            ProvidedConstructor(p.parameters, fun args -> p.extract args |> fst :> Expr)

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
