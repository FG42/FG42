document.addEventListener('DOMContentLoaded', () => {

  // Get all "navbar-burger" elements
  const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  // Check if there are any navbar burgers
  if ($navbarBurgers.length > 0) {

    // Add a click event on each of them
    $navbarBurgers.forEach( el => {
      el.addEventListener('click', () => {

        // Get the target from the "data-target" attribute
        const target = el.dataset.target;
        const $target = document.getElementById(target);

        // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
        el.classList.toggle('is-active');
        $target.classList.toggle('is-active');

      });
    });
  }

});
function imageshow(){
  document.getElementById('imagemodal').style.display = 'flex';
}
function imagehide(){
  document.getElementById('imagemodal').style.display = 'none';
}

document.onreadystatechange = function () {
  if (document.readyState == "complete") {
  var script = document.createElement('script'),
    scripts = document.getElementsByTagName('script')[0];
  script.src = "https://pouyacode.net/theme/js/fa.min.js?v=5.12.0";
  scripts.parentNode.insertBefore(script, scripts);

  // Email obfuscation using ROT13 for navbar.
  let rot13 = document.getElementById('email').href.replace(/[a-zA-Z]/g,function(c){return String.fromCharCode((c<="Z"?90:122)>=(c=c.charCodeAt(0)+13)?c:c-26);});
  document.getElementById('email').href = rot13;

  // Email obfuscation using ROT13 for articles.
  let subject = document.getElementsByClassName('article-title')[0].innerHTML;
  subject = subject.substr(subject.search('&nbsp')+6);
  rot13 += '?subject=' + subject;
  document.getElementById('feedback').innerHTML = '<a id="feedback" href="' + rot13 + '"><i class="fas fa-at"></i>&nbsp;Send feedback</a>';
  }
}
