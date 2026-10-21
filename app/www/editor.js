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
