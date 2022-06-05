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

type ProvidedStaticParameters<'T> =
    { Parameters: ProvidedStaticParameter list
      Extract: obj list -> 'T * obj list }

module ProvidedStaticParameters =

    let mandatory<'T> name =
        { Parameters = [ ProvidedStaticParameter(name, typeof<'T>) ]
          Extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }

    let optional<'T> name (defaultValue: 'T) =
        { Parameters = [ ProvidedStaticParameter(name, typeof<'T>, box defaultValue) ]
          Extract = function
            | :? 'T as x :: rest -> x, rest
            | args -> failwithf "Invalid args: %A" args }
        
    let ret<'T> (x: 'T) =
        { Parameters = []
          Extract = fun args -> x, args }
        
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedStaticParameters<'T>) =
        { Parameters = p.Parameters
          Extract = fun args ->
            let x, rest = p.Extract args
            f x, rest }

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
        
    let ret<'T> (x: 'T) =
        { Parameters = []
          Extract = fun args -> x, args }
        
    let map<'T, 'U> (f: 'T -> 'U) (p: ProvidedParameters<'T>) =
        { Parameters = p.Parameters
          Extract = fun args ->
            let x, rest = p.Extract args
            f x, rest }

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

    let addStaticParameters = AddStaticParametersBuilder()

    let addMembers (items: list<#MemberInfo>) (ty: ProvidedTypeDefinition) =
        ty.AddMembers items
        ty

    let addMember (items: MemberInfo) (ty: ProvidedTypeDefinition) =
        ty.AddMember items
        ty

    let addMembersDelayed (items: unit -> list<#MemberInfo>) (ty: ProvidedTypeDefinition) =
        ty.AddMembersDelayed items
        ty

    let addMemberDelayed (items: unit -> #MemberInfo) (ty: ProvidedTypeDefinition) =
        ty.AddMemberDelayed items
        ty

module ProvidedField =

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

    let dynInstanceMethod returnType name = MethodBuilder(name, returnType, false)

    let instanceMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, false)

    let dynStaticMethod returnType name = MethodBuilder(name, returnType, true)

    let staticMethod<'ReturnType> name = MethodBuilder<'ReturnType>(name, true)

    let constructor = ConstructorBuilder()
