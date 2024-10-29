package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.{
  ChorRun,
  Choreography,
  Class,
  Communicate,
  Constructor,
  Endpoint,
  InstanceField,
  Program,
  Variable,
}
import vct.col.rewrite.{Generation, Rewriter}

trait VeymontContext[Pre <: Generation] {

  object mappings {
    var program: Program[Pre] = null

    lazy val choreographies: Seq[Choreography[Pre]] =
      mappings.program.collect { case c: Choreography[Pre] => c }.toIndexedSeq

    lazy val classes: Seq[Class[Pre]] =
      mappings.program.collect { case cls: Class[Pre] => cls }.toIndexedSeq

    lazy val choreographyToCommunicates
        : Map[Choreography[Pre], Seq[Communicate[Pre]]] =
      choreographies.map { c =>
        (c, c.collect { case comm: Communicate[Pre] => comm }.toIndexedSeq)
      }.toMap
    lazy val runToChoreography: Map[ChorRun[Pre], Choreography[Pre]] =
      choreographies.map { chor => (chor.run, chor) }.toMap
    lazy val allEndpoints = choreographies.flatMap { _.endpoints }
    lazy val endpointToChoreography: Map[Endpoint[Pre], Choreography[Pre]] =
      choreographies.flatMap { chor => chor.endpoints.map(ep => (ep, chor)) }
        .toMap
    lazy val endpointClassToEndpoint: Map[Class[Pre], Endpoint[Pre]] =
      choreographies.flatMap { chor =>
        chor.endpoints.map(endpoint => (endpoint.cls.decl, endpoint))
      }.toMap

    lazy val constructorToClass: Map[Constructor[Pre], Class[Pre]] =
      classes.flatMap { cls =>
        cls.collect { case cons: Constructor[Pre] => (cons, cls) }
      }.toMap
    lazy val fieldToClass: Map[InstanceField[Pre], Class[Pre]] =
      classes.flatMap { cls =>
        cls.collect { case field: InstanceField[Pre] => (field, cls) }
      }.toMap
  }

  def communicatesOf(chor: Choreography[Pre]) =
    mappings.choreographyToCommunicates(chor)
  def communicatesOf(run: ChorRun[Pre]): Seq[Communicate[Pre]] =
    mappings.choreographyToCommunicates(mappings.runToChoreography(run))
  def communicatesOf(endpoint: Endpoint[Pre]): Seq[Communicate[Pre]] =
    communicatesOf(choreographyOf(endpoint))
      .filter(_.participants.contains(endpoint))
  def isEndpointClass(c: Class[Pre]): Boolean =
    mappings.endpointClassToEndpoint.contains(c)
  def choreographyOf(c: Class[Pre]): Choreography[Pre] =
    mappings.endpointToChoreography(mappings.endpointClassToEndpoint(c))
  def choreographyOf(endpoint: Endpoint[Pre]): Choreography[Pre] =
    mappings.endpointToChoreography(endpoint)
  def choreographyOf(run: ChorRun[Pre]): Choreography[Pre] =
    mappings.runToChoreography(run)
  def endpointOf(c: Class[Pre]): Endpoint[Pre] =
    mappings.endpointClassToEndpoint(c)
  def endpointsOf(run: ChorRun[Pre]) = choreographyOf(run).endpoints
  def isChoreographyParam(v: Variable[Pre]): Boolean =
    mappings.choreographies.exists { chor => chor.params.contains(v) }

  def classOf(cons: Constructor[Pre]): Class[Pre] =
    mappings.constructorToClass(cons)
  def classOfOpt(cons: Constructor[Pre]): Option[Class[Pre]] =
    mappings.constructorToClass.get(cons)
  def classOf(field: InstanceField[Pre]): Class[Pre] =
    mappings.fieldToClass(field)

  val currentChoreography = ScopedStack[Choreography[Pre]]()
  val currentEndpoint = ScopedStack[Endpoint[Pre]]()

  def inChoreography: Boolean =
    currentChoreography.nonEmpty && currentEndpoint.isEmpty
  def inEndpoint: Boolean =
    currentChoreography.nonEmpty && currentEndpoint.nonEmpty

  object InChor {
    def unapply[T](t: T): Option[(Choreography[Pre], T)] =
      if (inChoreography)
        Some((currentChoreography.top, t))
      else
        None
    def unapply: Option[Choreography[Pre]] =
      if (inChoreography)
        currentChoreography.topOption
      else
        None
  }

  object InEndpoint {
    def unapply[T](t: T): Option[(Choreography[Pre], Endpoint[Pre], T)] =
      if (inEndpoint)
        Some((currentChoreography.top, currentEndpoint.top, t))
      else
        None
    def unapply: Option[(Choreography[Pre], Endpoint[Pre])] =
      if (inEndpoint)
        Some((currentChoreography.top, currentEndpoint.top))
      else
        None
  }
}
