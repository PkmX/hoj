function escapeHtml(text) {
  var map = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#039;',
    " ": '&nbsp;'
  };

  return text.replace(/[&<>"' ]/g, function(m) { return map[m]; });
}

module.exports = escapeHtml;
