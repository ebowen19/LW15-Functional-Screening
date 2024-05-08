$(document).ready(function() {
    var urlParams = new URLSearchParams(window.location.search);
    var searchValue = urlParams.get('searchValue');

    var table = $('#singleGeneTable').DataTable({
        "ajax": {
            "url": "path_to_your_data_source", // Adjust this to your data source
            "dataSrc": ""
        },
        "columnDefs": [{
            "targets": "_all",
            "defaultContent": "Not available" // Default text if data is missing
        }],
        "initComplete": function(settings, json) {
            if (searchValue) {
                this.api().search(searchValue).draw();
            }
        }
    });

    // This forces the table to only display the searched row
    table.on('search.dt', function() {
        var filteredData = table.rows({ search: 'applied' }).data();
        if (filteredData.length === 1) {
            // Optionally do something when exactly one row is found
        } else {
            // Handle no match or multiple matches
            alert("No unique entry found. Showing closest matches.");
        }
    });
});
