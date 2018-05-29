window.urle = function(s){
  // neither escape nor encodeURIComponent get '*'
  var hex = "0123456789ABCDEF"
  return [].map.call(s,function(c){ switch(true) {
    case /[a-z0-9._~-]/.test(c): return c  // Safe
    case /[ -~]/.test(c):                  // Printable
      var n = c.charCodeAt(0)
      return "%"+hex[n/16 |0]+hex[n%16]
    default: return encodeURIComponent(c)  // Control, UTF8
}}).join('')};
