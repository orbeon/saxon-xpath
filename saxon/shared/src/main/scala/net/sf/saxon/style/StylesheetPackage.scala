package net.sf.saxon.style

import net.sf.saxon.utils.Configuration
import net.sf.saxon.expr.Component
import net.sf.saxon.expr.ComponentBinding
import net.sf.saxon.expr.PackageData
import net.sf.saxon.expr.instruct._
import net.sf.saxon.functions.ExecutableFunctionLibrary
import net.sf.saxon.functions.FunctionLibrary
import net.sf.saxon.functions.FunctionLibraryList
import net.sf.saxon.functions.registry.ConstructorFunctionLibrary
import net.sf.saxon.model.Affinity
import net.sf.saxon.model.TypeHierarchy
import net.sf.saxon.om.Action
import net.sf.saxon.om.SpaceStrippingRule
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.s9api.HostLanguage
import net.sf.saxon.serialize.CharacterMapIndex
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.trans._
/*import net.sf.saxon.trans.rules.RuleManager */// RuleManager not exist
import net.sf.saxon.tree.util.FastStringBuffer
import java.util._
import scala.jdk.CollectionConverters._
import net.sf.saxon.trans.Visibility.PRIVATE
import StylesheetPackage._
import net.sf.saxon.query.XQueryFunctionLibrary

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

object StylesheetPackage {

  private val TRACING: Boolean = false

}

class StylesheetPackage(config: Configuration) extends PackageData(config) {

  /*  @BeanProperty
    var packageVersion: PackageVersion = null  // PackageVersion not exist*/

  @BeanProperty
  var packageName: String = _

  @BeanProperty
  var usedPackages: List[StylesheetPackage] = new ArrayList()

  private var xsltVersion: Int = _
  /*
    @BeanProperty
    var ruleManager: RuleManager = _*/

  @BeanProperty
  var characterMapIndex: CharacterMapIndex = _

  @BooleanBeanProperty
  var createsSecondaryResultDocuments: Boolean = _

  private var completionActions: List[Action] = new ArrayList()

  var globalContextRequirement: GlobalContextRequirement = null

  private var containsGlobalContextItemDeclaration: Boolean = false

  var stripperRules: SpaceStrippingRule = _

  @BooleanBeanProperty
  var stripsWhitespace: Boolean = false

  @BooleanBeanProperty
  var stripsTypeAnnotations: Boolean = false

  var defaultOutputProperties: Properties = _

  @BeanProperty
  var defaultMode: StructuredQName = _

  @BooleanBeanProperty
  var declaredModes: Boolean = _

  var namedOutputProperties: Map[StructuredQName, Properties] =
    new HashMap(4)

  var schemaIndex: Set[String] = new HashSet(10)

  @BeanProperty
  var functionLibrary: FunctionLibraryList = _

  private var queryFunctions: XQueryFunctionLibrary = _

  private var overriding: ExecutableFunctionLibrary = _

  private var underriding: ExecutableFunctionLibrary = _

  private var maxFunctionArity: Int = -1

  @BooleanBeanProperty
  var retainUnusedFunctions: Boolean = false

  @BooleanBeanProperty
  var implicitPackage: Boolean = _

  @BeanProperty
  var componentIndex: HashMap[SymbolicName, Component] = new HashMap(20)

  var hiddenComponents: List[Component] = new ArrayList()

  var overriddenComponents: HashMap[SymbolicName, Component] =
    new HashMap()

  @BeanProperty
  var abstractComponents: HashMap[SymbolicName, Component] = new HashMap()

  this.hostLanguage = HostLanguage.XSLT

  this.accumulatorRegistry = config.makeAccumulatorRegistry

  def addUsedPackage(pack: StylesheetPackage): Unit = {
    usedPackages.add(pack)
  }

  def contains(pack: StylesheetPackage): Boolean =
    usedPackages.asScala
      .find(p => p == pack || p.contains(pack))
      .map(_ => true)
      .getOrElse(false)

  def setVersion(version: Int): Unit = {
    this.xsltVersion = version
  }

  def getVersion: Int = xsltVersion

  def isJustInTimeCompilation: Boolean = false

  def setJustInTimeCompilation(justInTimeCompilation: Boolean): Unit = ()

  def getSpaceStrippingRule: SpaceStrippingRule = stripperRules

  def getStripperRules: SpaceStrippingRule = stripperRules

  def setStripperRules(stripperRules: SpaceStrippingRule): Unit = {
    this.stripperRules = stripperRules
  }

  def setDefaultOutputProperties(props: Properties): Unit = {
    defaultOutputProperties = props
  }

  def setNamedOutputProperties(name: StructuredQName,
                               props: Properties): Unit = {
    namedOutputProperties.put(name, props)
  }

  def getNamedOutputProperties(name: StructuredQName): Properties =
    namedOutputProperties.get(name)

  def getSchemaNamespaces: Set[String] = schemaIndex

  def setContextItemRequirements(requirement: GlobalContextRequirement): Unit = {
    if (containsGlobalContextItemDeclaration) {
      if ((!requirement.isAbsentFocus && globalContextRequirement.isAbsentFocus) ||
        (requirement.isMayBeOmitted && !globalContextRequirement.isMayBeOmitted)) {
        throw new XPathException(
          "The package contains two xsl:global-context-item declarations with conflicting @use attributes",
          "XTSE3087")
      }
      val th: TypeHierarchy = getConfiguration.getTypeHierarchy
      if (th.relationship(requirement.getRequiredItemType,
        globalContextRequirement.getRequiredItemType) !=
        Affinity.SAME_TYPE) {
        throw new XPathException(
          "The package contains two xsl:global-context-item declarations with conflicting item types",
          "XTSE3087")
      }
    }
    containsGlobalContextItemDeclaration = true
    globalContextRequirement = requirement
  }

  def getContextItemRequirements: GlobalContextRequirement =
    globalContextRequirement

  def addCompletionAction(action: Action): Unit = {
    completionActions.add(action)
  }

  def complete(): Unit = {
    for (a <- completionActions.asScala) {
      a.doAction()
    }
    allocateBinderySlots()
  }

  def allocateBinderySlots(): Unit = {
    val slotManager: SlotManager = getConfiguration.makeSlotManager
    for (c <- componentIndex.values.asScala) {
      registerGlobalVariable(c, slotManager)
    }
    for (c <- hiddenComponents.asScala) {
      registerGlobalVariable(c, slotManager)
    }
    this.globalSlotManager = slotManager
  }

  private def registerGlobalVariable(c: Component,
                                     slotManager: SlotManager): Unit = {
    if (c.getActor.isInstanceOf[GlobalVariable]) {
      val `var`: GlobalVariable = c.getActor.asInstanceOf[GlobalVariable]
      val slot: Int = slotManager.allocateSlotNumber(`var`.getVariableQName)
      `var`.setPackageData(this)
      `var`.setBinderySlotNumber(slot)
      if (c.getVisibility != Visibility.HIDDEN) {
        addGlobalVariable(`var`)
      }
    }
  }

  def addComponent(component: Component): Unit = {
    val name: SymbolicName = component.getActor.getSymbolicName
    componentIndex.put(name, component)
    if (component.getVisibility == Visibility.ABSTRACT && component.getContainingPackage == this) {
      abstractComponents.put(component.getActor.getSymbolicName, component)
    }
  }

  override def addGlobalVariable(variable: GlobalVariable): Unit = {
    super.addGlobalVariable(variable)
    val name: SymbolicName = variable.getSymbolicName
    if (componentIndex.get(name) == null) {
      var comp: Component = variable.getDeclaringComponent
      if (comp == null) {
        comp = variable.makeDeclaringComponent(PRIVATE, this)
      }
      addComponent(comp)
    }
  }

  def getMaxFunctionArity: Int = {
    if (maxFunctionArity == -1) {
      for (c <- componentIndex.values.asScala if c.getActor.isInstanceOf[UserFunction]
           if c.getActor
             .asInstanceOf[UserFunction]
             .getArity > maxFunctionArity) {
        maxFunctionArity = c.getActor.asInstanceOf[UserFunction].getArity
      }
    }
    maxFunctionArity
  }

  def getComponent(name: SymbolicName): Component = componentIndex.get(name)

  def addHiddenComponent(component: Component): Unit = {
    hiddenComponents.add(component)
  }

  def getOverriddenComponent(name: SymbolicName): Component =
    overriddenComponents.get(name)

  def addOverriddenComponent(comp: Component): Unit = {
    overriddenComponents.put(comp.getActor.getSymbolicName, comp)
  }

  def addComponentsFromUsedPackage(usedPackage: StylesheetPackage /*,
                                   acceptors: List[XSLAccept]*/ , // XSLAccept not exist
                                   overrides: Set[SymbolicName]): Unit = {
    usedPackages.add(usedPackage)
    trace(
      "=== Adding components from " + usedPackage.getPackageName +
        " to " +
        getPackageName +
        " ===")
    val correspondence: Map[Component, Component] =
      new HashMap[Component, Component]()
    for ((key, value) <- usedPackage.componentIndex.asScala) {
      val name: SymbolicName = key
      val oldC: Component = value
      val oldV: Visibility.Visibility = oldC.getVisibility
      var newV: Visibility.Visibility = null
      if (overrides.contains(name) && !(oldC.getActor.isInstanceOf[Mode])) {
        newV = Visibility.HIDDEN
      } else {
        var acceptedVisibility: Visibility.Visibility =
          explicitAcceptedVisibility(name)
        if (acceptedVisibility != null) {
          /*if (!XSLAccept.isCompatible(oldV, acceptedVisibility)) {
            throw new XPathException(
              "Cannot accept a " + oldV.show() + " component (" + name +
                ") from package " +
                usedPackage.getPackageName +
                " with visibility " +
                acceptedVisibility.show(),
              "XTSE3040"
            )
          }*/
          newV = acceptedVisibility
        } else {
          acceptedVisibility = wildcardAcceptedVisibility(name)
          /* if (acceptedVisibility != null) {
             if (XSLAccept.isCompatible(oldV, acceptedVisibility)) {
               newV = acceptedVisibility
             }
           }*/
        }
        if (newV == null) {
          newV =
            if (oldV == Visibility.PUBLIC || oldV == Visibility.FINAL)
              Visibility.PRIVATE
            else Visibility.HIDDEN
        }
      }
      trace(
        oldC.getActor.getSymbolicName.toString + " (" + oldV.show().toString + ") becomes " +
          newV.show().toString)
      val newC: Component = Component.makeComponent(
        oldC.getActor,
        newV,
        VisibilityProvenance.DERIVED,
        this,
        oldC.getDeclaringPackage)
      correspondence.put(oldC, newC)
      newC.setBaseComponent(oldC)
      if (overrides.contains(name)) {
        overriddenComponents.put(name, newC)
        if (newV != Visibility.ABSTRACT) {
          abstractComponents.remove(name)
        }
      }
      if (newC.getVisibility == Visibility.HIDDEN) {
        hiddenComponents.add(newC)
      } else if (componentIndex.get(name) != null) {
        if (!(oldC.getActor.isInstanceOf[Mode])) {
          throw new XPathException("Duplicate " + key,
            "XTSE3050",
            oldC.getActor)
        }
      } else {
        componentIndex.put(name, newC)
        if (oldC.getActor.isInstanceOf[Mode] &&
          (oldV == Visibility.PUBLIC || oldV == Visibility.FINAL)) {
          /* val existing: Mode =
             getRuleManager.obtainMode(name.getComponentName, false)*/
          /*  if (existing != null) {
              throw new XPathException("Duplicate " + key,
                "XTSE3050",
                oldC.getActor)
            } else {}*/
        }
      }
      if (newC.getActor.isInstanceOf[Mode] && overrides.contains(name)) {
        addCompletionAction(() => {
          trace("Doing mode completion for " + newC.getActor.getSymbolicName)
          val oldBindings: List[ComponentBinding] =
            newC.getBaseComponent.getComponentBindings
          val newBindings: List[ComponentBinding] = newC.getComponentBindings
          for (i <- 0 until oldBindings.size) {
            val name12: SymbolicName = oldBindings.get(i).getSymbolicName
            var target: Component = null
            if (overrides.contains(name12)) {
              target = getComponent(name12)
              if (target == null) {
                throw new AssertionError(
                  "We know there's an override for " + name12 + ", but we can't find it")
              }
            } else {
              target = correspondence.get(oldBindings.get(i).getTarget)
              if (target == null) {
                throw new AssertionError(
                  "Saxon can't find the new component corresponding to " +
                    name12)
              }
            }
            val newBinding: ComponentBinding =
              new ComponentBinding(name12, target)
            newBindings.set(i, newBinding)
          }
        })
      } else {
        addCompletionAction(() => {
          trace("Doing normal completion for " + newC.getActor.getSymbolicName)
          val oldBindings: List[ComponentBinding] =
            newC.getBaseComponent.getComponentBindings
          val newBindings: List[ComponentBinding] =
            new ArrayList[ComponentBinding](oldBindings.size)
          makeNewComponentBindings(overrides,
            correspondence,
            oldBindings,
            newBindings)
          newC.setComponentBindings(newBindings)
        })
      }
    }
    for (oldC <- usedPackage.hiddenComponents.asScala) {
      trace(
        oldC.getActor.getSymbolicName.toString + " (HIDDEN, declared in " +
          oldC.getDeclaringPackage.getPackageName.toString +
          ") becomes HIDDEN")
      val newC: Component = Component.makeComponent(
        oldC.getActor,
        Visibility.HIDDEN,
        VisibilityProvenance.DERIVED,
        this,
        oldC.getDeclaringPackage)
      correspondence.put(oldC, newC)
      newC.setBaseComponent(oldC)
      hiddenComponents.add(newC)
      addCompletionAction(() => {
        val oldBindings: List[ComponentBinding] =
          newC.getBaseComponent.getComponentBindings
        val newBindings: List[ComponentBinding] =
          new ArrayList[ComponentBinding](oldBindings.size)
        makeNewComponentBindings(overrides,
          correspondence,
          oldBindings,
          newBindings)
        newC.setComponentBindings(newBindings)
      })
    }
    if (usedPackage.isCreatesSecondaryResultDocuments) {
      this.createsSecondaryResultDocuments = true
    }
  }

  private def makeNewComponentBindings(
                                        overrides: Set[SymbolicName],
                                        correspondence: Map[Component, Component],
                                        oldBindings: List[ComponentBinding],
                                        newBindings: List[ComponentBinding]): Unit = {
    for (oldBinding <- oldBindings.asScala) {
      val name: SymbolicName = oldBinding.getSymbolicName
      var target: Component = null
      if (overrides.contains(name)) {
        target = getComponent(name)
        if (target == null) {
          throw new AssertionError(
            "We know there's an override for " + name + ", but we can't find it")
        }
      } else {
        target = correspondence.get(oldBinding.getTarget)
        if (target == null) {
          throw new AssertionError(
            "Saxon can't find the new component corresponding to " +
              name)
        }
      }
      val newBinding: ComponentBinding = new ComponentBinding(name, target)
      newBindings.add(newBinding)
    }
  }

  private def trace(message: String): Unit = {
    if (TRACING) {
      System.err.println(message)
    }
  }

  private def explicitAcceptedVisibility(
                                          name: SymbolicName /*,
                                          acceptors: List[XSLAccept]*/): Visibility.Visibility = {
    /*for (acceptor <- acceptors.asScala; test <- acceptor.getExplicitComponentTests
         if test.matches(name)) {
      acceptor.getVisibility
    }*/
    null
  }

  private def wildcardAcceptedVisibility(
                                          name: SymbolicName /*,
                                          acceptors: List[XSLAccept]*/): Visibility.Visibility = {
    var vis: Visibility.Visibility = null
    /*   for (acceptor <- acceptors.asScala; test <- acceptor.getWildcardComponentTests // XSLAccept not exist
            if test.getQNameTest.asInstanceOf[NodeTest].getDefaultPriority ==
              -0.25 &&
              test.matches(name)) {
         vis = acceptor.getVisibility
       }*/
    if (vis != null) {
      return vis
    }
    /* for (acceptor <- acceptors.asScala; test <- acceptor.getWildcardComponentTests
          if test.matches(name)) {
       vis = acceptor.getVisibility
     }*/
    vis
  }

  def createFunctionLibrary(): Unit = {
    val functionLibrary: FunctionLibraryList = new FunctionLibraryList()
    val includeHOF: Boolean =
      !("HE" == getTargetEdition || "JS" == getTargetEdition)
    /*functionLibrary.addFunctionLibrary(
      if (includeHOF) config.getXSLT30FunctionSet
      else new Configuration().getXSLT30FunctionSet)
    functionLibrary.addFunctionLibrary(
      new StylesheetFunctionLibrary(this, true))*/
    functionLibrary.addFunctionLibrary(config.getBuiltInExtensionLibraryList)
    functionLibrary.addFunctionLibrary(new ConstructorFunctionLibrary(config))
    if ("JS" == getTargetEdition) {
      addIxslFunctionLibrary(functionLibrary)
    }
    queryFunctions = new XQueryFunctionLibrary(config)
    functionLibrary.addFunctionLibrary(queryFunctions)
    functionLibrary.addFunctionLibrary(config.getIntegratedFunctionLibrary)
    config.addExtensionBinders(functionLibrary)
    /*functionLibrary.addFunctionLibrary(
      new StylesheetFunctionLibrary(this, false))*/
    this.functionLibrary = functionLibrary
  }

  def addIxslFunctionLibrary(functionLibrary: FunctionLibraryList): Unit = ()

  /*def getPublicFunctions(): FunctionLibrary =
    new PublicStylesheetFunctionLibrary(functionLibrary)*/

  def getXQueryFunctionLibrary: XQueryFunctionLibrary = queryFunctions

  def setFunctionLibraryDetails(
                                 library: FunctionLibraryList,
                                 overriding: ExecutableFunctionLibrary,
                                 underriding: ExecutableFunctionLibrary): Unit = {
    if (library != null) {
      this.functionLibrary = library
    }
    this.overriding = overriding
    this.underriding = underriding
  }

  def getFunction(name: SymbolicName.F): UserFunction =
    if (name.getArity == -1) {
      val maximumArity: Int = 20
      for (a <- 0 until maximumArity) {
        val sn: SymbolicName.F = new SymbolicName.F(name.getComponentName, a)
        val uf: UserFunction = getFunction(sn)
        if (uf != null) {
          uf.incrementReferenceCount()
          uf
        }
      }
      null
    } else {
      val component: Component = getComponentIndex.get(name)
      if (component != null) {
        val uf: UserFunction = component.getActor.asInstanceOf[UserFunction]
        uf.incrementReferenceCount()
        uf
      } else {
        null
      }
    }

  def setRetainUnusedFunctions(): Unit = {
    this.retainUnusedFunctions = true
  }

  def updatePreparedStylesheet(/*pss: PreparedStylesheet*/): Unit = {
    for ((key, value) <- componentIndex.asScala
         if value.getVisibility == Visibility.ABSTRACT) {
      abstractComponents.put(key, value)
    }
    /* pss.setTopLevelPackage(this)
     if (isSchemaAware || !schemaIndex.isEmpty) {
       pss.setSchemaAware(true)
     }
     pss.setHostLanguage(HostLanguage.XSLT)*/
    val libraryList: FunctionLibraryList = new FunctionLibraryList()
    for (lib <- functionLibrary.getLibraryList.asScala) {
      /*    if (lib.isInstanceOf[StylesheetFunctionLibrary]) {
            if (lib
              .asInstanceOf[StylesheetFunctionLibrary]
              .isOverrideExtensionFunction) {
              libraryList.addFunctionLibrary(overriding)
            } else {
              libraryList.addFunctionLibrary(underriding)
            }
          } else {*/
      libraryList.addFunctionLibrary(lib)
      //}
    }
    /*pss.setFunctionLibrary(libraryList)
    if (!pss.createsSecondaryResult()) {
      pss.setCreatesSecondaryResult(mayCreateSecondaryResultDocuments())
    }*/
    /*pss.setDefaultOutputProperties(defaultOutputProperties)
    for ((key, value) <- namedOutputProperties.asScala) {
      pss.setOutputProperties(key, value)
    }
    if (characterMapIndex != null) {
      for (cm <- characterMapIndex.asScala) {
        pss.getCharacterMapIndex.putCharacterMap(cm.getName, cm)
      }
    }
    pss.setRuleManager(ruleManager)*/
    for (comp <- componentIndex.values.asScala
         if comp.getActor.isInstanceOf[NamedTemplate]) {
      val t: NamedTemplate = comp.getActor.asInstanceOf[NamedTemplate]
      // pss.putNamedTemplate(t.getTemplateName, t)
    }
    //pss.setComponentIndex(componentIndex)
    for (comp <- componentIndex.values.asScala
         if comp.getActor.isInstanceOf[GlobalParam]) {
      val gv: GlobalParam = comp.getActor.asInstanceOf[GlobalParam]
      // pss.registerGlobalParameter(gv)
    }
    /* if (globalContextRequirement != null) {
       pss.setGlobalContextRequirement(globalContextRequirement)
     }*/
  }

  private def mayCreateSecondaryResultDocuments(): Boolean = {
    if (createsSecondaryResultDocuments) {
      return true
    }
    for (p <- usedPackages.asScala if p.mayCreateSecondaryResultDocuments()) {
      true
    }
    false
  }

  def export(presenter: ExpressionPresenter): Unit = {
    throw new XPathException("Exporting a stylesheet requires Saxon-EE")
  }

  def checkForAbstractComponents(): Unit = {
    for ((key, value) <- componentIndex.asScala
         if value.getVisibility == Visibility.ABSTRACT && value.getContainingPackage == this) {
      abstractComponents.put(key, value)
    }
    if (!abstractComponents.isEmpty) {
      val buff: FastStringBuffer = new FastStringBuffer(256)
      var count: Int = 0
      breakable {
        for (name <- abstractComponents.keySet.asScala) {
          if ( {
            count += 1;
            count - 1
          } > 0) {
            buff.append(", ")
          }
          buff.append(name.toString)
          if (buff.length > 300) {
            buff.append(" ...")
            break()
          }
        }
      }
      throw new XPathException(
        "The package is not executable, because it contains abstract components: " +
          buff,
        "XTSE3080")
    }
  }

  def isFallbackToNonStreaming: Boolean = true

  def setFallbackToNonStreaming(): Unit = ()

}
