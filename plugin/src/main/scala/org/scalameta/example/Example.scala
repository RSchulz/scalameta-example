package org.scalameta.example

import scala.tools.nsc.CompilationUnits
import scala.{meta => api}
import scala.meta.internal.ast._
import scala.meta.semantic._
import scala.meta.semantic.errors.throwExceptions

import scala.language.implicitConversions

trait Example {
  val global: scala.tools.nsc.Global
  implicit val host = scala.meta.internal.hosts.scalac.Scalahost(global)

  import scala.meta.internal.ast._

  def example(unit: CompilationUnits#CompilationUnit, sources: List[Source]): Unit = {
    println()
    sources foreach { src =>
      println(s"Compilation Unit for '${unit.source.path}' {")
//    println(s"src=$src")
      src.stats foreach (render(_, 1))
    }
    println("}")
    println()
  }


  import TreeEnhancements._
  import Rendering._

  def render(t: api.Tree): Unit = render(t, 0)

  def render(t: api.Tree, depth: Int): Unit =
    t match {
      case p @ Pkg(ref, stats) =>
        if (p.hasBraces)
          renderOpen(s"pkg: ref=$ref; hasBraces=${p.hasBraces}; statements (${stats.length}):", depth)
        else
          renderln(s"pkg: ref=$ref; hasBraces=${p.hasBraces}; statements (${stats.length}):", depth)
        render(stats, depth + 1)
        if (p.hasBraces)
          renderClose(depth)

      case i @ Import(clauses) =>
        render("import:", clauses, depth)

      case ic @ Import.Clause(Term.Name(name), sels) =>
        renderln(s"import '$name':", depth)
        render("selector:", sels, depth + 1)

      case ic @ Import.Clause(ref, sels) =>
        renderln(s"import:", depth)
        render("ref:", ref, depth + 1)
        render("selector:", sels, depth + 1)

      case in @ Import.Name(name: String) =>
        renderln(s"impName: '$name'", depth)

      case ir @ Import.Rename(from: String, to: String) =>
        renderln(s"impRename: from='$from'; to='$to'", depth)

      case iw @ Import.Wildcard() =>
        renderln("import-wildcard", depth)

      case ta @ Type.Apply(tipe, tparams) =>
        renderOpen("type application:", depth)
        render("type constructor:", tipe, depth + 1)
        render("type params:", tparams, depth + 1)
        renderClose(depth)

      case ts @ Type.Select(qual, selector) =>
        renderln("type-select:", depth)
        render("qual:", qual, depth + 1)
        render("selector:", selector, depth + 1)

      case tar @ Type.Arg.Repeated(tipe) =>
        render("repeatedParam:", tipe, depth)

      case tb @ Type.Bounds(lo, hi) =>
        if (tb.isBounded) {
          renderln(s"type-bounds:", depth)
          lo.map(l => render("lo:", l, depth + 1))
          hi.map(h => render("hi:", h, depth + 1))
      }

      case tn @ Type.Name(name) =>
        renderln(s"name '$name'${"; backquted" ? tn.isBackquoted}:", depth)

      case tp @ Type.Param(mods, name, tparams, contextBounds, viewBounds, typeBounds) =>
        if (tp.nonTrivial) {
          renderOpen(s"type-param${name ? " '%s'"}:", depth)
          render("mods:", mods, depth + 1)
          render("tparams:", tparams, depth + 1)
          render("cboudns:", contextBounds, depth + 1)
          if (typeBounds.isBounded)
            render("tbounds:", typeBounds, depth + 1)
          renderClose(depth)
        }
        else
          renderln(s"type-param${name ? " '%s'"}", depth)

      case tc @ Type.Compound(types, refinements) =>
        renderln("type-compound:", depth)
        render("base types:", types, depth + 1)
        render("refinements:", refinements, depth + 1)

      case dd @ Decl.Def(mods, name, tparams, paramss, declType) =>
        if (dd.nonTrivial) {
          renderOpen(s"def-abstract '$name':", depth)
          render("mods:", mods, depth)
          render("tparams:", tparams, depth)
          renderParamss(paramss, depth)
          renderln(s"declType:", depth); render(declType, depth + 2)
          renderClose(depth)
        }
        else {
          renderln(s"def-abstract '$name'", depth)
          renderln(s"declType:", depth + 1); render(declType, depth + 2)
        }

      case dv @ Decl.Var(mod, pats, declType) =>
        renderln(s"decl-var:", depth)
        render("pats:", pats, depth + 1)
        render("declType:", declType, depth + 1)

      case dt @ Decl.Type(mods, name, tparams, bounds) =>
        renderln(s"decl-type '${name.value}':", depth)
        render("mods:", mods, depth + 1)
        render("tparams:", tparams, depth + 1)
        if (bounds.isBounded)
          render("bounds:", bounds, depth + 1)

      case dt @ Defn.Type(mods, name, tparams, body) =>
        renderOpen(s"defn-type '${name.value}:", depth)
        render("mods:", mods, depth + 1)
        render("tparams:", tparams, depth + 1)
        renderClose(depth + 1)
        renderClose(depth)

      case dt @ Defn.Trait(mods, name, tparams, templ) =>
        renderOpen(s"trait '$name':", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        render("templ:", templ, depth + 1)
        renderClose(depth)

      case dc @ Defn.Class(mods, name, tparams, ctor, templ) =>
        if (dc.nonTrivial) {
          renderOpen(s"class '$name':", depth)
          render("mods:", mods, depth + 1)
          render("tparams:", tparams, depth + 1)
          render("ctor:", ctor, depth + 1)
          render("templ:", templ, depth + 1)
          renderClose(depth)
        }
        else
          renderln(s"class '$name'", depth)

      case dO @ Defn.Object(mods, name, templ) =>
        renderOpen(s"object: '${name.value}", depth)
        render("mods:", mods, depth)
        render("templ:", templ, depth + 1)
        renderClose(depth)


//    case mc @ Mod.Case() =>
//      renderln(s"mod: case", depth)

      case ma @ Mod.Annot(ref) =>
        renderln("annotation:", depth)
        render("ctor-ref:", ref, depth + 1)

      case pw @ Mod.PrivateWithin(name) =>
        renderln(s"private-within: name='$name'", depth)

      case pw @ Mod.ProtectedWithin(name) =>
        renderln(s"protected-within: name='$name'", depth)

      case mc: Mod =>
        renderln(s"mod: ${mc.productPrefix}", depth)

      case t @ Templ(early, parents, self, stats) =>
        if (t.nonTrivial) {
//        renderln("templ:", depth)
          render("early:", early, depth)
          render("parents:", parents, depth)
          if (self.nonTrivial)
            render("self:", self, depth)
          render(s"statements: (${stats.length})", stats, depth)
        }
        else
          renderln("vacuous", depth)

      case cp @ Ctor.Primary(mods, paramss) =>
        if (cp.nonTrivial) {
          renderOpen("ctorPrimary:", depth)
          render(s"mods:", mods, depth + 1)
          renderParamss(paramss, depth + 1)
          renderClose(depth)
        }

      case cr @ Ctor.Ref(tipe, argss) =>
        render("type:", tipe, depth)
        renders("argss", argss, depth)

      case tpv @ Templ.Param.Val(mods, name, declType, dflt) =>
        renderOpen(s"templParamVal${name ? " '%s'"}:", depth)
        render("mods:", mods, depth + 1)
        declType map (t => render("declType:", t, depth + 1))
        dflt map (t => render("dflt:", t, depth + 1))
        renderClose(depth)

      case dd @ Defn.Def(mods, name, tparams, paramss, declType, body) =>
        renderOpen(s"def-implemented: '${name.value}'", depth)
        render("mods:", mods, depth + 1)
        render("tparams:", tparams, depth + 1)
        renderParamss(paramss, depth + 1)
        declType map { t => renderln(s"declType:", depth + 1); render(t, depth + 2) }
        render("body:", body, depth + 1)
        renderClose(depth)

      case dv @ Defn.Val(mods, pats, declType, rhs) =>
        renderOpen("val:", depth)
        render("mods:", mods, depth + 1)
        render("pats:", pats, depth + 1)
        declType map { t => renderln(s"declType:", depth + 1); render(t, depth + 2) }
        render("rhs:", rhs, depth + 1)
        renderClose(depth)

      case dv @ Defn.Var(mods, pats, declType, rhs) =>
        if (dv.nonTrivial) {
          renderOpen("var:", depth)
          render("mods:", mods, depth + 1)
          render("pats:", pats, depth + 1)
          declType map { t => renderln(s"declType:", depth + 1); render(t, depth + 2) }
          rhs.map(t => render("rhs:", t, depth + 1))
          renderClose(depth)
        }
        else
          renderln("var ???", depth)

      case tp @ Term.Param(mods, name, declType, dfltVal) =>
        renderOpen(s"param${name ? " '%s'"}:", depth)
        render(s"mods:", mods, depth)
        declType map { t => renderln(s"declType:", depth + 1); render(t, depth + 2) }
        dfltVal  map { v => renderln(s"dfltVal:", depth + 1);  render(v, depth + 2) }
        renderClose(depth)

      case tn @ Term.New(templ) =>
        render("new:", templ, depth)

      case ta @ Term.Apply(fun, args) =>
        render("apply:", fun, depth)
        render("args:", args, depth)

      case tai @ Term.ApplyInfix(lhs, op, targs, args) =>
        renderln("applyInfix:", depth)
        render("targs:", targs, depth + 1)
        render("lhs:", lhs, depth + 1)
        render("op:", op, depth + 1)
        render("args:", args, depth + 1)

      case ts @ Term.Select(qual, selector) =>
        renderOpen("select:", depth)
        render("qual:", qual, depth + 1)
        renderln(s"selector: '${selector.value}'${"; postfix" ? ts.isPostfix}", depth + 1)
        renderClose(depth)

      case tn @ Term.Name(value) =>
        renderln(s"name: '$value'${" (backquoted)" ? tn.isBackquoted}", depth)

      case ti @ Term.Interpolate(prefix, parts, args) =>
        renderOpen(s"interpolate: prefix.name=${prefix.value}", depth)
        render("parts", parts, depth + 1)
        render("args:", args, depth + 1)
        renderClose(depth)

      case pt @ Pat.Tuple(elements) =>
        render("pattern tuple elements:", elements, depth)

      case pt @ Pat.Typed(lhs, rhs) =>
        renderOpen("type pattern:", depth)
        render("lhs:", lhs, depth + 1)
        render("rhs:", rhs, depth + 1)
        renderClose(depth)

      case tt @ Term.Tuple(elements) =>
        renderOpen(s"tuple${elements.length}:", depth)
        render("elements:", elements, depth + 1)
        renderClose(depth)

      case pw @ Pat.Wildcard() =>
        renderln(s"wildcard pattern: _", depth)


      case tb @ Term.Block(stats) =>
        renderOpen(s"block:", depth)
        render(s"statements (${stats.length}):", stats, depth + 1)
        renderClose(depth)

      case tm @ Term.Match(scrutinee, cases) =>
        renderOpen("match:", depth)
        render("scrutinee:", scrutinee, depth + 1)
        render("cases:", cases, depth)
        renderClose(depth)

      case tfy @ Term.ForYield(enums, body) =>
        renderOpen("for-yield:", depth)
        render("enums:", enums, depth)
        render("body:", body, depth)
        renderClose(depth)

      case ta @ Term.Assign(lhs, rhs) =>
        renderln("assign:", depth)
        render("lhs:", lhs, depth)
        render("rhs:", rhs, depth)

      case eg @ Enum.Generator(pat, rhs) =>
        renderOpen("for generator:", depth)
        render("pat:", pat, depth + 1)
        render("rhs:", rhs, depth + 1)
        renderClose(depth)

      case tw @ Term.While(cond, body) =>
        renderOpen("while", depth)
        render("condition:", cond, depth)
        render("body:", body, depth)
        renderClose(depth)

      case c @ Case(pat, cond, stats) =>
        renderOpen("case:", depth)
        render("pat:", pat, depth + 1)
        cond.map(c => render("condition:", c, depth + 1))
        render("statements (${stats.length}):", stats, depth + 1)
        renderClose(depth)

      case lit: Lit =>
        renderln(s"literal: $lit (${lit.getClass.getSimpleName})", depth)

      case _ =>
        renderln(s"Other Tree (${t.getClass.getName}): «$t»", depth)
    }


  def renderOpen(intro: String, depth: Int): Unit =
    renderln(s"$intro {", depth)

  def renderClose(depth: Int, coda: String = ""): Unit =
    renderln(s"} $coda", depth)

  def renderParamss(paramss: Seq[Seq[Templ.Param]], depth: Int): Unit =
    if (paramss.nonEmpty) {
      renderln(s"paramss (${paramss.length})", depth)
      var nth = 0
      paramss foreach { params =>
        renderln(f"$nth%2d:", depth + 1); nth += 1
        render(params, depth + 2)
      }
    }


  def renders(pre: String, tss: Seq[Seq[api.Tree]], depth: Int): Unit = {
    if (tss.nonEmpty) {
      renderln(pre, depth)

      var nth = 0
      tss foreach { ts =>
        renderln(s"$nth", depth)
        nth += 1
        ts foreach (t => render(t, depth + 1))
      }
    }
  }

  def render(ts: Seq[api.Tree], depth: Int): Unit =
    ts foreach (t => render(t, depth))

  def render(pre: String, t: api.Tree, depth: Int): Unit = {
    renderln(pre, depth)
    render(t, depth + 1)
  }

  def render(pre: String, ts: Seq[api.Tree], depth: Int): Unit =
    if (ts.nonEmpty) {
      renderln(pre, depth)
      ts foreach (t => render(t, depth + 1))
    }

  def render(s: String): Unit = print(s)

  def renderln(s: String): Unit = println(s)

  def render(s: String, depth: Int): Unit =
    print(renderDepth(depth, s))

  def renderln(s: String, depth: Int): Unit =
    println(renderDepth(depth, s))

  def renderDepth(depth: Int, s: String): String = {
    def more(toGo: Int, soFar: List[String]): List[String] =
      toGo match {
        case 0 => s :: soFar
        case 1 => more(0, "– " :: soFar)
        case _ => more(toGo - 1, "––" :: soFar)
      }

    more(depth, Nil).reverse.mkString
  }
}


object TreeEnhancements {
  implicit class TypeBounds(val bounds: Type.Bounds) extends AnyVal {
    def isBounded: Boolean = bounds.lo.isDefined || bounds.hi.isDefined
  }

  implicit class TypeParam(val tparam: Type.Param) extends AnyVal {
    def nonTrivial: Boolean = {
      val Type.Param(mods, _, tparams, contextBounds, viewBounds, typeBounds) = tparam

      mods.nonEmpty ||
      tparams.nonEmpty ||
      contextBounds.nonEmpty ||
      viewBounds.nonEmpty ||
      typeBounds.isBounded
    }
  }

  implicit class DeclDef(val declDef: Decl.Def) extends AnyVal {
    def nonTrivial: Boolean = {
      val Decl.Def(mods, name, tparams, paramss, declType) = declDef

      mods.nonEmpty ||
      tparams.nonEmpty ||
      paramss.nonEmpty
    }
  }

  implicit class CtorPrimary(val ctor: Ctor.Primary) extends AnyVal {
    def nonTrivial: Boolean =
      ctor.mods.nonEmpty ||
      ctor.paramss.nonEmpty
  }

  implicit class TemplE(val templ: Templ) extends AnyVal {
    def nonTrivial: Boolean = {
      val Templ(early, parents, _, stats) = templ

      early.nonEmpty ||
      parents.nonEmpty ||
      stats.nonEmpty
    }
  }

  implicit class DefnClass(val defnClass: Defn.Class) extends AnyVal {
    def nonTrivial: Boolean = {
      val Defn.Class(mods, name, tparams, ctor, templ) = defnClass

      mods.nonEmpty ||
      tparams.nonEmpty ||
      tparams.nonEmpty ||
      ctor.nonTrivial ||
      templ.nonTrivial
    }
  }

  implicit class DefnVar(val defnVar: Defn.Var) extends AnyVal {
    def nonTrivial: Boolean = {
      val Defn.Var(mods, pats, declType, rhs) = defnVar

      mods.nonEmpty ||
      pats.nonEmpty ||
      declType.nonEmpty ||
      rhs.nonEmpty
      }
    }

  implicit class TermParam(val termParam: Term.Param) extends AnyVal {
    def nonTrivial: Boolean = {
      val Term.Param(mods, name, declType, default) = termParam

      mods.nonEmpty ||
      name.nonEmpty ||
      declType.nonEmpty ||
      default.nonEmpty
    }
  }
}


object Rendering {
  implicit class StringIf(s: String) {
    def ? (cond: Boolean): String =
      if (cond)
        s
      else
        ""

    override def toString = s
  }

  implicit class StringOpt(val s: Option[String]) extends AnyVal {
//  def this(s: String) = this(Some(s))

    def ? (fmt: String): String =
      s.map(s => fmt.format(s)).getOrElse("")

    override def toString = s.getOrElse("")
  }

  implicit def SOForOptName(on: Option[Name]): StringOpt =
    on match {
      case Some(tn) => new StringOpt(Some(tn.value))
      case None     => new StringOpt(None)
    }
}
