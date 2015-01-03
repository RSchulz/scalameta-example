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
      println(s"Compilation Unit for '${unit.source.path}':")
//    println(s"src=$src")
      src.stats foreach render
    }
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
          renderln(s"pkg: ref=$ref; hasBraces=${p.hasBraces}; statements (${stats.length}7):", depth)
        render(stats, depth + 1)
        if (p.hasBraces)
          renderClose(depth)

      case i @ Import(clauses) =>
        render("import:", clauses, depth)

      case ic @ Import.Clause(ref, sels) =>
        render("impClause:", sels, depth)

      case in @ Import.Name(name: String) =>
        renderln(s"impName: '$name'", depth)

      case ir @ Import.Rename(from: String, to: String) =>
        renderln(s"impRename: from='$from'; to='$to'", depth)

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
        renderln(s"name: name='$name'${"; backquted" ? tn.isBackquoted}", depth)

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

      case dd @ Decl.Def(mods, name, tparams, paramss, declType) =>
        renderOpen(s"def-abstract: '$name':", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        renderParamss(paramss, depth)
        renderln(s"declType:", depth); render(declType, depth + 1)
        renderClose(depth)

      case dt @ Decl.Type(mods, name, tparams, bounds) =>
        renderln(s"decl-type '${name.value}':", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        if (bounds.isBounded)
          render("bounds:", bounds, depth)

      case dt @ Defn.Type(mods, name, tparams, body) =>
        renderOpen(s"defn-type '${name.value}:", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        render("body:", body, depth)
        renderClose(depth)

      case dt @ Defn.Trait(mods, name, tparams, templ) =>
        renderOpen(s"trait '$name':", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        render("templ:", templ, depth + 1)
        renderClose(depth)

      case dc @ Defn.Class(mods, name, tparams, ctor, templ) =>
        renderOpen(s"class '$name':", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        render("ctor:", ctor, depth)
        render("templ:", templ, depth + 1)
        renderClose(depth)

      case dO @ Defn.Object(mods, name, templ) =>
        renderOpen(s"object: '${name.value}", depth)
        render("mods:", mods, depth)
        render("templ:", templ, depth + 1)
        renderClose(depth)


//    case mc @ Mod.Case() =>
//      renderln(s"mod: case", depth)

      case mc: Mod =>
        renderln(s"mod: ${mc.productPrefix}", depth)

      case t @ Templ(early, parents, self, stats) =>
        render("templEarly:", early, depth)
        render("parents:", parents, depth)
        render("self:", self, depth)
        render(s"statements: (${stats.length})", stats, depth)

      case cp @ Ctor.Primary(mods, paramss) =>
        renderOpen("ctorPrimary:", depth)
        render(s"mods:", mods, depth)
        renderParamss(paramss, depth)
        renderClose(depth)

      case cr @ Ctor.Ref(tipe, argss) =>
        render("ctorType:", tipe, depth)
        renders("ctorArgss", argss, depth)

      case tpv @ Templ.Param.Val(mods, name, declType, dflt) =>
        renderOpen(s"templParamVal${name ? " ('%s')"}:", depth)
        render("mods:", mods, depth)
        declType map (t => render("declType:", t, depth + 1))
        dflt map (t => render("dflt:", t, depth + 1))
        renderClose(depth)

      case dd @ Defn.Def(mods, name, tparams, paramss, declType, body) =>
        renderOpen(s"def-implemented: '$name'", depth)
        render("mods:", mods, depth)
        render("tparams:", tparams, depth)
        renderParamss(paramss, depth)
        declType map { t => renderln(s"declType:", depth); render(t, depth + 1) }
        render("body:", body, depth)
        renderClose(depth)

      case dv @ Defn.Val(mods, pats, declType, rhs) =>
        renderOpen("val:", depth)
        render("mods:", mods, depth)
        render("pats:", pats, depth)
        declType map { t => renderln(s"declType:", depth); render(t, depth + 1) }
        render("rhs:", rhs, depth + 1)
        renderClose(depth)

      case tv @ Defn.Var(mods, pats, declType, rhs) =>
        renderOpen("var:", depth)
        render("mods:", mods, depth)
        render("pats:", pats, depth)
        declType map { t => renderln(s"declType:", depth); render(t, depth + 1) }
        rhs.map(t => render("rhs:", t, depth + 1))
        renderClose(depth)

      case tp @ Term.Param(mods, name, declType, dfltVal) =>
        renderOpen(s"param '$name':", depth)
        render(s"mods:", mods, depth)
        declType map { t => renderln(s"declType:", depth); render(t, depth + 1) }
        dfltVal  map { v => renderln(s"dfltVal:", depth);  render(v, depth + 1) }
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
        render("elements:", elements, depth)
        renderClose(depth)

      case pw @ Pat.Wildcard() =>
        renderln(s"wildcard pattern: _", depth)


      case tb @ Term.Block(stats) =>
        renderOpen(s"block:", depth)
        render("statements (${stats.length}):", stats, depth)
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
        renderln(s"literal: $lit", depth)

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

  implicit class StringOpt(s: Option[String]) {
    def this(s: String) = this(Some(s))

    def ? (fmt: String): String =
      s.map(s => fmt.format(s)).getOrElse("")

    override def toString = s.getOrElse("")
  }

  implicit def SOForOptName(on: Option[Name]): StringOpt =
    on match {
      case Some(tn) => new StringOpt(tn.value)
      case None     => new StringOpt("")
    }
}
