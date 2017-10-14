var Util = {};
Util.width = 0;
Util.height = 0;
Util.onload = () => {
  Util.cvs = document.getElementById('canvas');
  function resize()
  {
    var ctnr = document.getElementById('container');
    Util.width = Util.cvs.width = ctnr.clientWidth;
    Util.height = Util.cvs.height = ctnr.clientHeight;
  }
  resize();
  window.onresize = resize;
}