const toggle = document.querySelector('#navbar-toggle');
const menu = document.querySelector('#navbar');

toggle.addEventListener('click', function(_e) {
    console.log("hello");
  menu.classList.toggle('is-open');
});
