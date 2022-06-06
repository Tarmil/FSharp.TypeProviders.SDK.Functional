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

    /// Add XML documentation information to this provided parameter.
    let addXmlDoc doc (p: ProvidedStaticParameters<'T>) =
        p.parameters |> List.iter (fun p -> p.AddXmlDoc(doc))
        p

    /// Add XML documentation information to this provided parameter, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedStaticParameters<'T>) =
        p.parameters |> List.iter (fun p -> p.AddXmlDocDelayed(doc))
        p

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

type IProvidedParameterDescription<'This when 'This :> IProvidedParameterDescription<'This>> =
    abstract WithIsOut : bool -> 'This
    abstract WithIsParamArray : bool -> 'This
    abstract WithIsReflectedDefinition : bool -> 'This
    abstract WithCustomAttribute : CustomAttributeData -> 'This

/// Description of a provided method parameter.
type ProvidedParameterDescription<'T> =
    { name: string
      isOut: bool
      defaultValue: 'T option
      isParamArray: bool
      isReflectedDefinition: bool
      customAttributes: CustomAttributeData list }

    interface IProvidedParameterDescription<ProvidedParameterDescription<'T>> with
        member this.WithCustomAttribute(v) = { this with customAttributes = v :: this.customAttributes }
        member this.WithIsOut(v) = { this with isOut = v }
        member this.WithIsParamArray(v) = { this with isParamArray = v }
        member this.WithIsReflectedDefinition(v) = { this with isReflectedDefinition = v }

/// Description of a provided method parameter.
type ProvidedParameterDescription =
    { name: string
      parameterType: Type
      isOut: bool
      defaultValue: obj option
      isParamArray: bool
      isReflectedDefinition: bool
      customAttributes: CustomAttributeData list }

    interface IProvidedParameterDescription<ProvidedParameterDescription> with
        member this.WithCustomAttribute(v) = { this with customAttributes = v :: this.customAttributes }
        member this.WithIsOut(v) = { this with isOut = v }
        member this.WithIsParamArray(v) = { this with isParamArray = v }
        member this.WithIsReflectedDefinition(v) = { this with isReflectedDefinition = v }

/// A set of parameters for a provided method.
type ProvidedParameters<'T> =
    { parameters: ProvidedParameter list
      extract: Expr list -> 'T * Expr list }

let private coerce<'T> (e: Expr) =
    if e.Type = typeof<'T> then e else Expr.Coerce(e, typeof<'T>)
    |> Expr.Cast<'T>

let private extractOneArgWith f = function
    | e :: rest -> f e, rest
    | args -> failwithf "Invalid args: %A" args

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

    /// Indicates if the parameter is marked as ParamArray.
    let isParamArray v (p: #IProvidedParameterDescription<_>) =
        p.WithIsParamArray(v)

    /// Indicates if the parameter is marked as ReflectedDefinition.
    let isReflectedDefinition v (p: #IProvidedParameterDescription<_>) =
        p.WithIsReflectedDefinition(v)

    /// Add a custom attribute to the provided parameter.
    let addCustomAttribute a (p: #IProvidedParameterDescription<_>) =
        p.WithCustomAttribute(a)

    /// Create a "this" parameter for an instance method.
    let this<'T> =
        { parameters = []
          extract = extractOneArgWith coerce<'T> }

    /// Create a "this" parameter for an instance method.
    let dynThis =
        { parameters = []
          extract = extractOneArgWith id }

    let single (descr: ProvidedParameterDescription<'T>) =
        let p = ProvidedParameter(descr.name, typeof<'T>, descr.isOut,
            ?optionalValue = Option.map box descr.defaultValue,
            IsParamArray = descr.isParamArray,
            IsReflectedDefinition = descr.isReflectedDefinition)
        for a in descr.customAttributes do p.AddCustomAttribute(a)
        { parameters = [ p ]
          extract = extractOneArgWith coerce<'T> }

    let dynSingle descr =
        let p = ProvidedParameter(descr.name, descr.parameterType, descr.isOut,
            ?optionalValue = descr.defaultValue,
            IsParamArray = descr.isParamArray,
            IsReflectedDefinition = descr.isReflectedDefinition)
        for a in descr.customAttributes do p.AddCustomAttribute(a)
        { parameters = [ p ]
          extract = extractOneArgWith id }

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

    /// Add the given type as an implemented interface.
    let addInterfaceImplementation intf (ty: ProvidedTypeDefinition) =
        ty.AddInterfaceImplementation(intf)
        ty

    /// Add the given type as an implemented interface.
    let addInterfaceImplementationsDelayed intf (ty: ProvidedTypeDefinition) =
        ty.AddInterfaceImplementationsDelayed(intf)
        ty

    /// Specifies that the given method body implements the given method declaration.
    let addMethodOverride methodBody methodDeclaration (ty: ProvidedTypeDefinition) =
        ty.DefineMethodOverride(methodBody, methodDeclaration)
        ty

    /// Specifies that the given method body implements the given method declaration.
    let addMethodOverridesDelayed overrides (ty: ProvidedTypeDefinition) =
        ty.DefineMethodOverridesDelayed(overrides)
        ty

    /// Add a member to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    let addMemberDelayed (items: unit -> #MemberInfo) (ty: ProvidedTypeDefinition) =
        ty.AddMemberDelayed items
        ty

    /// Add XML documentation information to this provided type definition.
    let addXmlDoc doc (p: ProvidedTypeDefinition) =
        p.AddXmlDoc(doc)
        p

    /// Add XML documentation information to this provided type definition, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedTypeDefinition) =
        p.AddXmlDocDelayed(doc)
        p

    /// Add XML documentation information to this provided type definition, where the documentation is re-computed  every time it is required.
    let addXmlDocComputed doc (p: ProvidedTypeDefinition) =
        p.AddXmlDocComputed(doc)
        p

    /// Add definition location information to the provided type definition.
    let addDefinitionLocation line column filePath (p: ProvidedTypeDefinition) =
        p.AddDefinitionLocation(line, column, filePath)
        p

    /// Add a custom attribute to the provided type definition.
    let addCustomAttribute attr (p: ProvidedTypeDefinition) =
        p.AddCustomAttribute(attr)
        p

    /// Add a 'Obsolete' attribute to this provided type definition.
    let addObsoleteAttribute message isError (c: ProvidedTypeDefinition) =
        c.AddObsoleteAttribute(message, isError)
        c

    /// Set the attributes on the provided type. This fully replaces the default TypeAttributes.
    let setAttributes attrs (p: ProvidedTypeDefinition) =
        p.SetAttributes(attrs)
        p

    /// When creating a non-erased type provider, add the type definition to a provided assembly.
    let addToProvidedAssembly (asm: ProvidedAssembly) (ty: ProvidedTypeDefinition) =
        asm.AddTypes([ty])
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

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    let dynInstanceMethod returnType name = MethodBuilder(name, returnType, false)

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    let instanceMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, false)

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    let dynStaticMethod returnType name = MethodBuilder(name, returnType, true)

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    let staticMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, true)

    /// Add a 'Obsolete' attribute to this provided method.
    let addObsoleteAttribute message isError (c: ProvidedMethod) =
        c.AddObsoleteAttribute(message, isError)
        c

    /// Add XML documentation information to this provided method.
    let addXmlDoc doc (p: ProvidedMethod) =
        p.AddXmlDoc(doc)
        p

    /// Add XML documentation information to this provided method, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedMethod) =
        p.AddXmlDocDelayed(doc)
        p

    /// Add XML documentation information to this provided method, where the documentation is re-computed  every time it is required.
    let addXmlDocComputed doc (p: ProvidedMethod) =
        p.AddXmlDocComputed(doc)
        p

    /// Add method attributes to the method. By default these are simple 'MethodAttributes.Public'.
    let addMethodAttrs attrs (p: ProvidedMethod) =
        p.AddMethodAttrs(attrs)
        p

    /// Set the method attributes of the method. By default these are simple 'MethodAttributes.Public'.
    let setMethodAttrs attrs (p: ProvidedMethod) =
        p.SetMethodAttrs(attrs)
        p

    /// Add definition location information to the provided method definition.
    let addDefinitionLocation line column filePath (p: ProvidedMethod) =
        p.AddDefinitionLocation(line, column, filePath)
        p

    /// Add a custom attribute to the provided method definition.
    let addCustomAttribute attr (p: ProvidedMethod) =
        p.AddCustomAttribute(attr)
        p

    type AddStaticParametersBuilder() =
        member _.MergeSources(p1, p2) =
            ProvidedStaticParameters.map2 (fun x1 x2 -> (x1, x2)) p1 p2

        member _.BindReturn(p, f) =
            ProvidedStaticParameters.map f p

        member _.Return(x) =
            ProvidedStaticParameters.ret x

        member _.Run(p: ProvidedStaticParameters<string -> ProvidedMethod>) =
            fun (ty: ProvidedMethod) ->
                ty.DefineStaticParameters(p.parameters, fun methName args ->
                    let build, _ = p.extract (List.ofArray args)
                    build methName)
                ty

    /// Computation expression that adds static parameters to a ProvidedMethod by binding them with let!...and!.
    let addStaticParameters = AddStaticParametersBuilder()

module ProvidedConstructor =

    type ConstructorBuilder() =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<unit>>) =
            ProvidedConstructor(p.parameters, fun args -> p.extract args |> fst :> Expr)

    /// Computation expression that creates a constructor by binding its arguments with let!...and!.
    let constructor = ConstructorBuilder()

    /// Add a 'Obsolete' attribute to this provided constructor.
    let addObsoleteAttribute message isError (c: ProvidedConstructor) =
        c.AddObsoleteAttribute(message, isError)
        c

    /// Add XML documentation information to this provided constructor.
    let addXmlDoc doc (p: ProvidedConstructor) =
        p.AddXmlDoc(doc)
        p

    /// Add XML documentation information to this provided constructor, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedConstructor) =
        p.AddXmlDocDelayed(doc)
        p

    /// Add XML documentation information to this provided constructor, where the documentation is re-computed  every time it is required.
    let addXmlDocComputed doc (p: ProvidedConstructor) =
        p.AddXmlDocComputed(doc)
        p

    /// Set the target and arguments of the base constructor call. Only used for generated types.
    let baseConstructorCall call (p: ProvidedConstructor) =
        p.BaseConstructorCall <- call
        p

    /// Set a flag indicating that the constructor acts like an F# implicit constructor, so the
    /// parameters of the constructor become fields and can be accessed using Expr.GlobalVar with the
    /// same name.
    let isImplicitConstructor v (p: ProvidedConstructor) =
        p.IsImplicitConstructor <- v
        p

    let isTypeInitializer v (p: ProvidedConstructor) =
        p.IsTypeInitializer <- v
        p

    /// Add definition location information to the provided constructor.
    let addDefinitionLocation line column filePath (p: ProvidedConstructor) =
        p.AddDefinitionLocation(line, column, filePath)
        p

type ProvidedPropertyDescription =
    { get: unit -> Expr
      set: Expr -> Expr<unit> }

type ProvidedPropertyDescription<'T> =
    { get: unit -> Expr<'T>
      set: Expr<'T> -> Expr<unit> }

module ProvidedProperty =

    type IndexerGetBuilder(name: string, propertyType: Type, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr>) =
            ProvidedProperty(name, propertyType, isStatic = isStatic,
                indexParameters = p.parameters,
                getterCode = fun args -> p.extract args |> fst)

    type IndexerGetBuilder<'PropertyType>(name: string, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<'PropertyType>>) =
            ProvidedProperty(name, typeof<'PropertyType>, isStatic = isStatic,
                indexParameters = p.parameters,
                getterCode = fun args -> p.extract args |> fst :> Expr)

    type IndexerSetBuilder(name: string, propertyType: Type, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr -> Expr<unit>>) =
            ProvidedProperty(name, propertyType, isStatic = isStatic,
                indexParameters = p.parameters,
                setterCode = fun args ->
                    let f, args = p.extract args
                    let value, _ = extractOneArgWith id args
                    f value :> Expr)

    type IndexerSetBuilder<'PropertyType>(name: string, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<Expr<'PropertyType> -> Expr<unit>>) =
            ProvidedProperty(name, typeof<'PropertyType>, isStatic = isStatic,
                indexParameters = p.parameters,
                setterCode = fun args ->
                    let f, args = p.extract args
                    let value, _ = extractOneArgWith coerce<'PropertyType> args
                    f value :> Expr)

    type IndexerGetSetBuilder(name: string, propertyType: Type, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<ProvidedPropertyDescription>) =
            ProvidedProperty(name, propertyType, isStatic = isStatic,
                indexParameters = p.parameters,
                getterCode = (fun args -> (p.extract args |> fst).get()),
                setterCode = fun args ->
                    let f, args = p.extract args
                    let value, _ = extractOneArgWith id args
                    f.set value :> Expr)

    type IndexerGetSetBuilder<'PropertyType>(name: string, isStatic: bool) =
        inherit ProvidedParameters.ParametersBuilder()

        member _.Run(p: ProvidedParameters<ProvidedPropertyDescription<'PropertyType>>) =
            ProvidedProperty(name, typeof<'PropertyType>, isStatic = isStatic,
                indexParameters = p.parameters,
                getterCode = (fun args -> (p.extract args |> fst).get() :> Expr),
                setterCode = fun args ->
                    let f, args = p.extract args
                    let value, _ = extractOneArgWith coerce<'PropertyType> args
                    f.set value :> Expr)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynInstanceIndexerGet returnType name = IndexerGetBuilder(name, returnType, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynStaticIndexerGet returnType name = IndexerGetBuilder(name, returnType, true)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let instanceIndexerGet<'ReturnType> name = IndexerGetBuilder<'ReturnType>(name, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let staticIndexerGet<'ReturnType> name = IndexerGetBuilder<'ReturnType>(name, true)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynInstanceIndexerSet returnType name = IndexerSetBuilder(name, returnType, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynStaticIndexerSet returnType name = IndexerSetBuilder(name, returnType, true)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let instanceIndexerSet<'ReturnType> name = IndexerSetBuilder<'ReturnType>(name, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let staticIndexerSet<'ReturnType> name = IndexerSetBuilder<'ReturnType>(name, true)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynInstanceIndexerGetSet returnType name = IndexerGetSetBuilder(name, returnType, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let dynStaticIndexerGetSet returnType name = IndexerGetSetBuilder(name, returnType, true)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let instanceIndexerGetSet<'ReturnType> name = IndexerGetSetBuilder<'ReturnType>(name, false)

    /// Computation expression that creates an indexer by binding its arguments with let!...and!.
    let staticIndexerGetSet<'ReturnType> name = IndexerGetSetBuilder<'ReturnType>(name, true)

    /// Add a 'Obsolete' attribute to this provided property.
    let addObsoleteAttribute message isError (c: ProvidedProperty) =
        c.AddObsoleteAttribute(message, isError)
        c

    /// Add XML documentation information to this provided property.
    let addXmlDoc doc (p: ProvidedProperty) =
        p.AddXmlDoc(doc)
        p

    /// Add XML documentation information to this provided property, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedProperty) =
        p.AddXmlDocDelayed(doc)
        p

    /// Add XML documentation information to this provided property, where the documentation is re-computed  every time it is required.
    let addXmlDocComputed doc (p: ProvidedProperty) =
        p.AddXmlDocComputed(doc)
        p

    /// Add definition location information to the provided property.
    let addDefinitionLocation line column filePath (p: ProvidedProperty) =
        p.AddDefinitionLocation(line, column, filePath)
        p

    /// Add a custom attribute to the provided property.
    let addCustomAttribute attr (p: ProvidedProperty) =
        p.AddCustomAttribute(attr)
        p

module ProvidedEvent =

    /// Add XML documentation information to this provided event.
    let addXmlDoc doc (p: ProvidedEvent) =
        p.AddXmlDoc(doc)
        p

    /// Add XML documentation information to this provided event, where the computation of the documentation is delayed until necessary.
    let addXmlDocDelayed doc (p: ProvidedEvent) =
        p.AddXmlDocDelayed(doc)
        p

    /// Add XML documentation information to this provided event, where the documentation is re-computed  every time it is required.
    let addXmlDocComputed doc (p: ProvidedEvent) =
        p.AddXmlDocComputed(doc)
        p

    /// Add definition location information to the provided event.
    let addDefinitionLocation line column filePath (p:ProvidedEvent) =
        p.AddDefinitionLocation(line, column, filePath)
        p
