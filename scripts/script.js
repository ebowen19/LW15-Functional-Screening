$(document).ready(function() {
    var table = $('#functionalScreening').DataTable({
        order: [[5, 'desc']],
        lengthMenu: [[10, 25, 50, 100, -1], [10, 25, 50, 100, "All"]],
        pageLength: 25,
        autoWidth: false,
        scrollX: true,
        columnDefs: [
            { orderable: false, targets: [7, 8, 9, 10] }
        ],
        language: {
            lengthMenu: "Display _MENU_ genes per page"
        },
        initComplete: function() {
            var urlParams = new URLSearchParams(window.location.search);
            var searchValue = urlParams.get('searchValue');
            if (searchValue) {
                var self = this.api();
                self.search(searchValue).draw();

                self.one('draw', function() {
                    var pageIndex = self.rows({ search: 'applied' }).indexes().toArray();
                    if (pageIndex.length > 0) {
                        self.page(Math.floor(pageIndex[0] / self.page.len())).draw(false);
                    }

                    var node = self.rows({ search: 'applied' }).nodes();
                    if (node.length > 0) {
                        $('html, body').animate({
                            scrollTop: $(node).offset().top - 100
                        }, 1000);
                    }
                });
            }
        }
    });

    // Modal interaction setup for images in the DataTable
    var modal = document.getElementById("myModal");
    var modalImg = document.getElementById("img01");
    var captionText = document.getElementById("caption");
    var span = document.getElementsByClassName("close")[0];

    $('#functionalScreening').on('click', 'img', function() {
        modal.style.display = "block";
        modalImg.src = this.getAttribute('data-img-src') || this.src;
        captionText.innerHTML = this.alt || "Gene Plot";
    });

    span.onclick = function() {
        modal.style.display = "none";
    };

    modal.onclick = function(event) {
        if (event.target == modal) {
            modal.style.display = "none";
        }
    };

    // Dynamic adjustment of column visibility based on checkboxes
    $('input[type="checkbox"].column-visibility-toggle').change(function() {
        const columnMap = {
            'hideGeneID': 4,
            'hideCoverage': [2, 3],
            'hideRegulation': 1,
            'hideFoundIn': 5,
            'hideCategory': 6,
            'hideGraeber': 14
        };

        let columnIndexes = columnMap[this.id];
        if (!Array.isArray(columnIndexes)) {
            columnIndexes = [columnIndexes];
        }

        columnIndexes.forEach(index => {
            var column = table.column(index);
            column.visible(!column.visible());
        });
    });

    // Custom search functionality based on gene trend comparison
    $.fn.dataTable.ext.search.push(
        function(settings, data, dataIndex) {
            var sameTrend = $('#sameTrend').prop('checked');
            var difTrend = $('#difTrend').prop('checked');
            var trend = data[13]; // Assuming trend data is in the 14th column

            if ((sameTrend && trend === 'same') || (difTrend && trend === 'dif') || (!sameTrend && !difTrend)) {
                return true;
            }
            return false;
        }
    );

    // Re-draw the table when trend filters change
    $('#sameTrend, #difTrend').on('change', function() {
        table.draw();
    });
});
