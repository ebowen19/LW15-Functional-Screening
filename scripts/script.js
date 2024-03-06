// Get the modal
var modal = document.getElementById("myModal");

// Get the image and insert it inside the modal - use its "alt" text as a caption
var modalImg = document.getElementById("img01");

// Assuming your table has an ID of 'myTable'
var table = document.getElementById("functionalScreening");
var imgs = table.getElementsByTagName("img");

for (var i = 0; i < imgs.length; i++) {
  imgs[i].onclick = function(){
    modal.style.display = "block";
    modalImg.src = this.src;
    captionText.innerHTML = this.alt;
  }
}

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
span.onclick = function() { 
  modal.style.display = "none";
}

// When the user clicks anywhere outside of the modal content (the image, header, or download link), close the modal
modal.onclick = function(event) {
  if (event.target == modal) {
  modal.style.display = "none";
  }
  }

  // Add event listeners to the modal-trigger elements
document.querySelectorAll('.modal-trigger').forEach(function(element) {
  element.addEventListener('click', function() {
      var jpgSrc = this.getAttribute('data-img-src');
      var fullJpgSrc = 'images/' + jpgSrc;
      var tifSrc = fullJpgSrc.replace(".jpg", ".tif");

      modal.style.display = "flex";
      modalImg.src = fullJpgSrc;
      captionText.innerHTML = this.innerText;
      downloadLink.href = tifSrc;
  });
});

window.addEventListener('scroll', function() {
var element = document.getElementById('key');
element.style.right = 10 - window.pageXOffset + 'px';
});

$(document).ready(function() {
  $('#functionalScreening').DataTable({
      "order": [[5, 'desc']], // This line sets the default sort order.
      "lengthMenu": [[10, 25, 50, 100, -1], [10, 25, 50, 100, "All"]],
      "pageLength": 25, // This sets a default length of rows per page.
      "language": {
          "lengthMenu": "Display _MENU_ genes per page", // This line changes the text for the length menu.
      }
  });
});

$(document).ready(function() {
  $('.simpleTable').DataTable({
    "searching": false,
    "lengthChange": false,
    "bInfo": false // This will hide the "Showing 1 to n of n entries" info
  });
});
