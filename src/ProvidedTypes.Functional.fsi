module ProviderImplementation.ProvidedTypes.Functional

open System
open System.Reflection
open FSharp.Core.CompilerServices
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
type FunctionalProvider =
    inherit TypeProviderForNamespaces

    new : TypeProviderConfig
        * build: (Assembly -> list<ProvidedTypeDefinition>)
        * ?sourceAssemblies: Assembly list
        * ?assemblyReplacementMap: (string * string) list
        * ?addDefaultProbingLocation: bool
       -> FunctionalProvider


/// A set of static parameters for a type provider.
type ProvidedStaticParameters<'T> =
    { Parameters : ProvidedStaticParameter list
      Extract : obj list -> 'T * obj list }

module ProvidedStaticParameters =

    /// Create a mandatory static parameter with the given name.
    val mandatory<'T> : name: string -> ProvidedStaticParameters<'T>

    /// Create an optional static parameter with the given name and default value.
    val optional<'T> : name: string -> defaultValue: 'T -> ProvidedStaticParameters<'T>

    /// Create an empty set of static parameters.
    val ret<'T> : 'T -> ProvidedStaticParameters<'T>

    /// Map the value extracted from a set of static parameters.
    val map<'T, 'U>
        : ('T -> 'U)
        -> ProvidedStaticParameters<'T>
        -> ProvidedStaticParameters<'U>

    /// Combine two sets of static parameters.
    val map2<'T, 'U, 'V>
        : ('T -> 'U -> 'V)
        -> ProvidedStaticParameters<'T>
        -> ProvidedStaticParameters<'U>
        -> ProvidedStaticParameters<'V>

type ProvidedParameters<'T> =
    { Parameters: ProvidedParameter list
      Extract: Expr list -> 'T * Expr list }

module ProvidedParameters =

    /// Create a mandatory parameter with the given name.
    val mandatory<'T> : name: string -> ProvidedParameters<Expr<'T>>

    /// Create a mandatory parameter with the given name.
    val dynMandatory : ty: Type -> name: string -> ProvidedParameters<Expr>

    /// Create an optional parameter with the given name and default value.
    val optional<'T> : name: string -> defaultValue: 'T -> ProvidedParameters<Expr<'T>>

    /// Create an optional parameter with the given name and default value.
    val dynOptional : ty: Type -> name: string -> defaultValue: 'T -> ProvidedParameters<Expr>

    /// Create a "this" parameter for an instance method.
    val this<'T> : ProvidedParameters<Expr<'T>>

    /// Create a "this" parameter for an instance method.
    val dynThis : ProvidedParameters<Expr>

    /// Create an empty set of static parameters.
    val ret<'T> : 'T -> ProvidedParameters<'T>

    /// Map the value extracted from a set of static parameters.
    val map<'T, 'U>
        : ('T -> 'U)
        -> ProvidedParameters<'T>
        -> ProvidedParameters<'U>

    /// Combine two sets of static parameters.
    val map2<'T, 'U, 'V>
        : ('T -> 'U -> 'V)
        -> ProvidedParameters<'T>
        -> ProvidedParameters<'U>
        -> ProvidedParameters<'V>

    [<Class>]
    type ParametersBuilder =
        member MergeSources<'T, 'U>
            : ProvidedParameters<'T>
            * ProvidedParameters<'U>
            -> ProvidedParameters<'T * 'U>

        member BindReturn<'T, 'U>
            : ProvidedParameters<'T>
            * ('T -> 'U)
            -> ProvidedParameters<'U>

        member Return<'T> : 'T -> ProvidedParameters<'T>

    /// Computation expression that creates a method body by binding its arguments with let!...and!.
    val bind : ParametersBuilder

module ProvidedTypeDefinition =

    [<Class>]
    type AddStaticParametersBuilder =
        member MergeSources<'T, 'U>
            : ProvidedStaticParameters<'T>
            * ProvidedStaticParameters<'U>
            -> ProvidedStaticParameters<'T * 'U>

        member BindReturn<'T, 'U>
            : ProvidedStaticParameters<'T>
            * ('T -> 'U)
            -> ProvidedStaticParameters<'U>

        member Return<'T>
            : 'T
            -> ProvidedStaticParameters<'T>

        member Run
            : ProvidedStaticParameters<string -> ProvidedTypeDefinition>
            -> (ProvidedTypeDefinition -> ProvidedTypeDefinition)

        member Run
            : ProvidedStaticParameters<ProvidedTypeDefinition -> ProvidedTypeDefinition>
            -> (ProvidedTypeDefinition -> ProvidedTypeDefinition)

    /// Computation expression that adds static parameters to a ProvidedTypeDefinition by binding them with let!...and!.
    val addStaticParameters : AddStaticParametersBuilder

    /// Add a set of members to a ProvidedTypeDefinition.
    val addMembers
        : list<#MemberInfo>
       -> ProvidedTypeDefinition
       -> ProvidedTypeDefinition

    /// Add a method, property, nested type or other member to a ProvidedTypeDefinition.
    val addMember
        : MemberInfo
       -> ProvidedTypeDefinition
       -> ProvidedTypeDefinition

    /// Add a set of members to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    val addMembersDelayed
        : (unit -> list<#MemberInfo>)
       -> ProvidedTypeDefinition
       -> ProvidedTypeDefinition

    /// Add a member to a ProvidedTypeDefinition, delaying computation of the members until required by the compilation context.
    val addMemberDelayed
        : (unit -> #MemberInfo)
       -> ProvidedTypeDefinition
       -> ProvidedTypeDefinition

module ProvidedField =

    /// Create a new provided literal field. It is not initially associated with any specific provided type definition.
    val literal<'T>
        : name: string
       -> value: 'T
       -> ProvidedField

module ProvidedMethod =

    [<Class>]
    type MethodBuilder =
        inherit ProvidedParameters.ParametersBuilder

        member Run : ProvidedParameters<Expr> -> ProvidedMethod

    [<Class>]
    type MethodBuilder<'ReturnType> =
        inherit ProvidedParameters.ParametersBuilder

        member Run : ProvidedParameters<Expr<'ReturnType>> -> ProvidedMethod

    [<Class>]
    type ConstructorBuilder =
        inherit ProvidedParameters.ParametersBuilder

        member Run : ProvidedParameters<Expr<unit>> -> ProvidedConstructor

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    val dynInstanceMethod
        : returnType: Type
        -> name: string
        -> MethodBuilder

    /// Computation expression that creates an instance method by binding its arguments with let!...and!.
    val instanceMethod<'ReturnType>
        : name: string
        -> MethodBuilder<'ReturnType>

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    val dynStaticMethod
        : returnType: Type
        -> name: string
        -> MethodBuilder

    /// Computation expression that creates a static method by binding its arguments with let!...and!.
    val staticMethod<'ReturnType>
        : name: string
        -> MethodBuilder<'ReturnType>

    /// Computation expression that creates a constructor by binding its arguments with let!...and!.
    val constructor : ConstructorBuilder
