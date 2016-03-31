package okey
package format

import Piece._

class VisualTest extends OkeyTest {

  val f = Visual

  "The visual table formatter" should {
    "export new table" in {
      f.addNewLines(f >> (newTable)) must_== newTableFormat
    }
    "import new table" in {
      f << newTableFormat must_== newTable
    }
    "import and export is non destructive" in {
      forall(examples) { example =>
        f.addNewLines(f >> (f << example)) must_== example
      }
    }
  }

  val newTable = makeTable(R1, L1, G1, B1, R13, R2)
  val newTableFormat = """
r13
r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2r2
r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1r1
l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1l1
g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1g1
b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1
r1
l1
g1
b1
er10l10g10b10 wr11l11g11b11 wr12l12g12b12
sr10r10 sl10l10 sg10g10 sb10b10
"""

  val examples = Seq(newTableFormat,
  """
r1
r1r2r3
r1r2r3
r1r2r3
r1r2r3
r1r2r3






""",
    """
r1
r1r2r3







r1
wr1r2r3

"""
)

}
