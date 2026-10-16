"""Build the remaining synthetic teaching handout for Phase 4B."""

from __future__ import annotations

from pathlib import Path

from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import mm
from reportlab.platypus import (
    BaseDocTemplate,
    Frame,
    KeepTogether,
    PageTemplate,
    Paragraph,
    Spacer,
    Table,
    TableStyle,
)


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "static" / "files" / "demo"
OUTPUT.mkdir(parents=True, exist_ok=True)

NAVY = colors.HexColor("#102A43")
NAVY_DARK = colors.HexColor("#081D2F")
COPPER = colors.HexColor("#A94F2B")
TEAL = colors.HexColor("#0D6B67")
MIST = colors.HexColor("#EEF3F5")
MUTED = colors.HexColor("#536B7A")
PAPER = colors.HexColor("#FBFCFD")
WARNING = colors.HexColor("#FFF3CF")

styles = getSampleStyleSheet()
styles.add(ParagraphStyle(name="PreviewBanner", parent=styles["Normal"], fontName="Helvetica-Bold", fontSize=9, leading=12, textColor=colors.HexColor("#5B4300"), alignment=TA_CENTER, spaceAfter=8))
styles.add(ParagraphStyle(name="DocEyebrow", parent=styles["Normal"], fontName="Helvetica-Bold", fontSize=8, leading=10, textColor=COPPER, spaceAfter=5, uppercase=True))
styles.add(ParagraphStyle(name="DocTitle", parent=styles["Title"], fontName="Times-Roman", fontSize=25, leading=30, textColor=NAVY_DARK, spaceAfter=10))
styles.add(ParagraphStyle(name="DocSubtitle", parent=styles["Normal"], fontName="Times-Roman", fontSize=13, leading=18, textColor=MUTED, spaceAfter=12))
styles.add(ParagraphStyle(name="DocH2", parent=styles["Heading2"], fontName="Times-Roman", fontSize=16, leading=20, textColor=NAVY_DARK, spaceBefore=10, spaceAfter=6))
styles.add(ParagraphStyle(name="DocBody", parent=styles["BodyText"], fontName="Helvetica", fontSize=9.5, leading=14, textColor=NAVY, spaceAfter=8))
styles.add(ParagraphStyle(name="DocSmall", parent=styles["Normal"], fontName="Helvetica", fontSize=7.5, leading=11, textColor=MUTED))


def on_page(canvas, doc):
    width, height = A4
    canvas.saveState()
    canvas.setFillColor(PAPER)
    canvas.rect(0, 0, width, height, fill=1, stroke=0)
    canvas.setFillColor(NAVY_DARK)
    canvas.rect(0, height - 14 * mm, width, 14 * mm, fill=1, stroke=0)
    canvas.setFillColor(colors.white)
    canvas.setFont("Helvetica-Bold", 8)
    canvas.drawString(18 * mm, height - 9 * mm, "ANDERSON VIL | PHASE 4B PRIVATE PREVIEW")
    canvas.setFillColor(COPPER)
    canvas.rect(0, 0, width, 3 * mm, fill=1, stroke=0)
    canvas.setFillColor(MUTED)
    canvas.setFont("Helvetica", 7)
    canvas.drawRightString(width - 18 * mm, 8 * mm, f"Preview page {doc.page}")
    canvas.restoreState()


def make_doc(path: Path, title: str, subtitle: str, sections: list[tuple[str, str]], metadata: list[tuple[str, str]]):
    doc = BaseDocTemplate(str(path), pagesize=A4, leftMargin=18 * mm, rightMargin=18 * mm, topMargin=24 * mm, bottomMargin=16 * mm, title=title, author="Synthetic preview content", subject="Synthetic demonstration content - not for publication")
    frame = Frame(doc.leftMargin, doc.bottomMargin, doc.width, doc.height, id="normal")
    doc.addPageTemplates(PageTemplate(id="preview", frames=frame, onPage=on_page))
    story = []
    banner = Table([[Paragraph("SYNTHETIC DEMONSTRATION CONTENT - NOT FOR PUBLICATION", styles["PreviewBanner"])]], colWidths=[doc.width])
    banner.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, -1), WARNING), ("BOX", (0, 0), (-1, -1), .6, colors.HexColor("#DFC46D")), ("TOPPADDING", (0, 0), (-1, -1), 7), ("BOTTOMPADDING", (0, 0), (-1, -1), 1)]))
    story.extend([banner, Spacer(1, 8), Paragraph("PREVIEW DOCUMENT", styles["DocEyebrow"]), Paragraph(title, styles["DocTitle"]), Paragraph(subtitle, styles["DocSubtitle"])])
    meta_table = Table([[Paragraph(f"<b>{key}</b>", styles["DocSmall"]), Paragraph(value, styles["DocSmall"])] for key, value in metadata], colWidths=[34 * mm, doc.width - 34 * mm])
    meta_table.setStyle(TableStyle([("BACKGROUND", (0, 0), (-1, -1), MIST), ("GRID", (0, 0), (-1, -1), .35, colors.HexColor("#CBD7DD")), ("VALIGN", (0, 0), (-1, -1), "TOP"), ("LEFTPADDING", (0, 0), (-1, -1), 7), ("RIGHTPADDING", (0, 0), (-1, -1), 7), ("TOPPADDING", (0, 0), (-1, -1), 6), ("BOTTOMPADDING", (0, 0), (-1, -1), 6)]))
    story.extend([meta_table, Spacer(1, 10)])
    for heading, body in sections:
        story.append(KeepTogether([Paragraph(heading, styles["DocH2"]), Paragraph(body, styles["DocBody"])]))
    story.extend([Spacer(1, 12), Paragraph("Technical flags: synthetic: true | placeholder: true | publicationStatus: blocked", styles["DocSmall"]), Paragraph("Replacement required before production. See data/preview_inventory.toml in the site repository.", styles["DocSmall"])])
    doc.build(story)


make_doc(
    OUTPUT / "synthetic-applied-economics-handout.pdf",
    "Applied Economics Lab: Association and Causal Claims",
    "Fictional 90-minute workshop handout for navigation and download testing.",
    [
        ("Learning objective", "Distinguish a descriptive association from a causal estimate, state the assumptions required for interpretation, and rewrite an overconfident conclusion."),
        ("Synthetic prompt", "Two fictional groups have different average market-work and care hours. Compute each mean, describe the difference, then list at least three reasons why the comparison does not identify a causal effect."),
        ("Discussion sequence", "1. Define the unit and variables. 2. Reproduce the group means. 3. Draw a causal diagram. 4. Identify missing variables. 5. Write a cautious conclusion."),
        ("Download pair", "Use synthetic_households.json with analyze_demo.py or demo_notebook.ipynb. The expected group means are documented in the project README."),
        ("Instructor note", "No institution, real course, student record, or empirical research result is represented in this handout."),
    ],
    [("Language", "English"), ("Level", "Synthetic undergraduate workshop"), ("Institution", "None"), ("Publication", "Blocked")],
)

print(f"Created the remaining synthetic teaching PDF in {OUTPUT}")
