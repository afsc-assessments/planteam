(() => {
  let sectionKey = null;
  let metadataFmp = null;
  let dirty = false;
  let loading = false;
  const sectionFields = {text: "section_text", table_tsv: "section_table_tsv", caption: "section_caption", source: "section_source", source_year: "section_source_year", reviewed: "section_reviewed", include: "section_include"};
  const metaFields = ["year", "title", "contributors", "document_date", "catch_data_date", "source"];
  const value = id => {
    const node = document.getElementById(id);
    return node.type === "checkbox" ? node.checked : node.value;
  };
  const set = (id, value) => {
    const node = document.getElementById(id);
    if (node.type === "checkbox") node.checked = Boolean(value);
    else node.value = value;
  };
  const markDirty = () => { dirty = true; };
  function sendSection(event) {
    if (loading || !sectionKey) return;
    if (event && !["section_reviewed", "section_include"].includes(event.target.id)) set("section_reviewed", false);
    markDirty();
    Shiny.setInputValue("section_edit", {key: sectionKey, section: Object.fromEntries(Object.entries(sectionFields).map(([key, id]) => [key, value(id)])), nonce: Date.now()}, {priority: "event"});
  }
  function sendMetadata() {
    if (loading || !metadataFmp) return;
    markDirty();
    Shiny.setInputValue("metadata_edit", {fmp: metadataFmp, metadata: Object.fromEntries(metaFields.map(key => [key, value("meta_" + key)])), nonce: Date.now()}, {priority: "event"});
  }
  document.addEventListener("DOMContentLoaded", () => {
    Object.values(sectionFields).forEach(id => {
      document.getElementById(id).addEventListener("input", sendSection);
      document.getElementById(id).addEventListener("change", sendSection);
    });
    metaFields.forEach(key => document.getElementById("meta_" + key).addEventListener("input", sendMetadata));
    Shiny.addCustomMessageHandler("builder-section", data => {
      loading = true;
      sectionKey = data.key;
      Object.entries(sectionFields).forEach(([key, id]) => set(id, data[key]));
      loading = false;
    });
    Shiny.addCustomMessageHandler("builder-metadata", data => {
      loading = true;
      metadataFmp = data.fmp;
      metaFields.forEach(key => set("meta_" + key, data[key]));
      loading = false;
    });
    Shiny.addCustomMessageHandler("builder-saved", saved => { dirty = !saved; });
  });
  window.addEventListener("beforeunload", event => {
    if (dirty) { event.preventDefault(); event.returnValue = "Changes may still be saving. Use Save now before closing."; }
  });
})();


// Static help never reads or changes the working document.
(() => {
  const hints = {
    fmp: "Choose BSAI or GOA. Both documents stay in the same working JSON; each has its own details.",
    topic: "Choose a stock, or Front matter, appendices and tables. Switching views keeps your edits.",
    component: "Choose the regional component. Whole stock holds shared material; available components depend on the FMP and stock.",
    section: "Choose the subsection to edit. Check the heading before pasting into a different stock or region.",
    include_stock: "Include every subsection for this stock without changing its text. Choose a stock first.",
    exclude_stock: "Omit this stock from rendered documents. Its text stays in the working JSON.",
    meta_year: "Use a four-digit report year. Every included subsection's source-review year must match it for a for-review render.",
    meta_title: "Document title for the selected FMP. Replace defaults as needed for this report.",
    meta_contributors: "List the current contributors or authors. This field is required for a for-review render.",
    meta_document_date: "Enter the document date or meeting label. The app does not supply or verify an adopted meeting date.",
    meta_catch_data_date: "State the date through which the catch data are current. No catch data are fetched automatically.",
    meta_source: "Record the source, accepted run or approval reference for this document. The app does not verify scientific approval.",
    save_now: "Save both FMPs to local working.json. Wait for Saved locally before closing or refreshing.",
    reopen: "Reload the disk copy. Download your session's working copy first if a save failed or another session changed the file.",
    download_json: "Download the complete editable JSON for both FMPs. Keep this backup before imports or recovery.",
    new_document: "Start new TODO scaffolds for both FMPs. The current document is saved and backed up first; download a copy for long-term retention.",
    section_include: "Include this subsection in exports. Turning this off preserves its text in the working JSON.",
    section_text: "Paste current scientific narrative with Cmd+V. TODO text is a prompt; pasted markup and code render literally.",
    copy_section: "Copy the narrative, table, caption and source fields into this session's internal subsection clipboard.",
    cut_section: "Copy this subsection internally, then clear its content. Undo cut / paste restores the cleared subsection.",
    paste_section: "Replace the target with the internally copied subsection and reset its review flag. Check stock and region first.",
    undo_edit: "Restore the most recent internal cut or paste, including its original target. This is one level of undo, not typing history.",
    section_caption: "Supply a descriptive table caption and units. The app does not infer or convert units.",
    section_table_tsv: "Paste a header plus data rows as tab-separated columns. Use 2–30 unique headers and equal-width rows; no formulas are evaluated.",
    section_source: "Record the accepted source, model run or approval reference supporting this subsection.",
    section_source_year: "Enter the report year for which you reviewed this source. It must match Document details, not necessarily the source's publication year.",
    section_reviewed: "Mark after checking this content for the selected report year. Editing content or pasting a subsection clears this flag.",
    render_mode: "Draft allows missing or stale content and labels it. For review requires the checklist to pass; it is not release approval.",
    render_formats: "HTML supports QA/QC; Word and PDF support document review. Select at least one. PDF needs XeLaTeX and Times New Roman.",
    render: "Save both FMPs, then render only the selected FMP into a new _output/builder bundle. Read the status and any failed-format log.",
    open_json: "Import a validated builder JSON, replacing both FMPs after saving the current document. Download a working copy first."
  };
  document.addEventListener("DOMContentLoaded", () => {
    const heading = document.querySelector(".app-heading");
    if (!heading) return;
    const actions = document.createElement("div");
    actions.className = "heading-actions";
    actions.append(heading.querySelector(".save-status"));
    const help = document.createElement("a");
    help.id = "builder_help";
    help.className = "btn btn-default";
    help.href = "help.html";
    help.target = "_blank";
    // Same-origin static help can return to this exact editor without reloading it.
    help.rel = "opener";
    help.textContent = "Help & hints ↗";
    help.setAttribute("aria-label", "Help and hints (opens a separate tab)");
    actions.append(help);
    heading.append(actions);
    const note = document.createElement("p");
    note.className = "help-text hint-instructions";
    note.textContent = "Hover over a control or focus it with Tab for a hint. Escape dismisses it.";
    heading.firstElementChild.append(note);

    let active = null;
    const hide = (force = false) => {
      if (!active) return;
      const {anchor, targets, tip} = active;
      if (!force && (anchor.matches(":hover") || tip.matches(":hover") || targets.includes(document.activeElement))) return;
      tip.hidden = true;
      active = null;
    };
    const position = () => {
      if (!active) return;
      const {anchor, tip} = active;
      const box = anchor.getBoundingClientRect();
      const gap = 4;
      tip.style.left = Math.max(8, Math.min(box.left, window.innerWidth - tip.offsetWidth - 8)) + "px";
      // Prefer above: a focused narrative must not cover the clipboard buttons below it.
      const above = box.top - tip.offsetHeight - gap;
      tip.style.top = Math.max(8, above >= 8 ? above : Math.min(box.bottom + gap, window.innerHeight - tip.offsetHeight - 8)) + "px";
    };
    Object.entries(hints).forEach(([id, text]) => {
      const anchor = document.getElementById(id);
      if (!anchor) return;
      const tip = document.createElement("div");
      tip.id = "hint-" + id;
      tip.className = "builder-tooltip";
      tip.role = "tooltip";
      tip.hidden = true;
      tip.textContent = text;
      document.body.append(tip);
      const targets = anchor.matches("input,textarea,select,button,a") ? [anchor] : [...anchor.querySelectorAll("input,button,select")];
      targets.forEach(target => target.setAttribute("aria-describedby", [target.getAttribute("aria-describedby"), tip.id].filter(Boolean).join(" ")));
      const show = () => {
        hide(true);
        active = {anchor, targets, tip};
        tip.hidden = false;
        position();
      };
      anchor.addEventListener("mouseenter", show);
      anchor.addEventListener("mouseleave", () => setTimeout(hide, 120));
      targets.forEach(target => {
        target.addEventListener("focus", show);
        target.addEventListener("blur", () => setTimeout(hide, 120));
      });
      tip.addEventListener("mouseleave", () => setTimeout(hide, 120));
    });
    document.addEventListener("keydown", event => { if (event.key === "Escape") hide(true); });
    // Tab changes and collapsed table panels must not leave an orphaned hint.
    document.addEventListener("click", event => {
      if (event.target.closest('[data-toggle="tab"], summary')) hide(true);
    });
    document.addEventListener("scroll", position, true);
    window.addEventListener("resize", position);
  });
})();
