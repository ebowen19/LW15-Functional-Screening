

$.fn.dataTable.ext.type.order['scientific-pre'] = function(data) {
    console.log('Sorting data:', data);
    return parseFloat(data);
};

// load in sorted p-value order
async function fetchSortedGeneList() {
    const response = await fetch('../TCGA/sorted_genes.csv');
    const data = await response.text();
    const geneList = data.split('\n').map(line => line.trim()).filter(line => line.length);
    return geneList;
}



document.addEventListener('DOMContentLoaded', async function() {
  // Modal setup
  var modal = document.getElementById("myModal");
  var modalImg = document.getElementById("img01");
  var captionText = document.getElementById("caption");
  var span = document.getElementsByClassName("close")[0]; // Close button for the modal

  document.getElementById("functionalScreening").addEventListener('click', function(event) {
      var element = event.target;
      if (element.tagName === 'IMG') {
          modal.style.display = "block";
          modalImg.src = element.getAttribute('data-img-src') || element.src;
          captionText.innerHTML = element.alt || "Gene Plot";
      }
  });

  span.onclick = function() {
      modal.style.display = "none";
  };

  modal.onclick = function(event) {
      if (event.target == modal) {
          modal.style.display = "none";
      }
  };

  // Adjusting the 'key' element on scroll
  window.addEventListener('scroll', function() {
      var keyElement = document.getElementById('key');
      keyElement.style.right = 10 - window.pageXOffset + 'px';
  });

  // Fetch the sorted gene list
  const sortedGenes = await fetchSortedGeneList();


  // DataTables setup
  var table = $('#functionalScreening').DataTable({
      order: [[5, 'desc']],
      lengthMenu: [[10, 25, 50, 100, -1], [10, 25, 50, 100, "All"]],
      pageLength: 25,
      language: {
          lengthMenu: "Display _MENU_ genes per page"
      },
      autoWidth: false,
      scrollX: true,
      "columnDefs": [
        { "orderable": true, 
        "targets": -1 }]
  });


  // Adjust table width dynamically
  function adjustTableWidth() {
      var totalWidth = 0;
      table.columns().every(function() {
          if (this.visible()) {
              totalWidth += $(this.header()).outerWidth();
          }
      });

      var containerWidth = $('#functionalScreening_wrapper').width();
      $('#functionalScreening').css('width', Math.min(totalWidth, containerWidth));
      table.columns.adjust().draw();
  }

  // Adjust width on checkbox change
  $('input[type="checkbox"]').change(function() {
      const columnMap = {
          'hideGeneID': 4,
          'hideCoverage': [2, 3],
          'hideRegulation': 1,
          'hideFoundIn': 5,
          'hideCategory': 6
      };

      let columnIndexes = columnMap[this.id];
      if (!Array.isArray(columnIndexes)) {
          columnIndexes = [columnIndexes];
      }

      columnIndexes.forEach(index => {
          var column = table.column(index);
          column.visible(!column.visible());
      });

      adjustTableWidth();
  });

  // Initialize simpleTable without searching and info display
  $('.simpleTable').DataTable({
      searching: false,
      lengthChange: false,
      bInfo: false
  });

  // Initial call to adjust table width
  adjustTableWidth();
});

