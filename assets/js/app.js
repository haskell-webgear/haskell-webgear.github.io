$(document).ready(function() {
    // Select first tab
    $(".code-tab li").removeClass("is-active");
    $(".code-tab li").first().addClass("is-active");

    // Show only first content
    $(".code-tab-content").hide();
    var selected = $(".code-tab li.is-active").first().attr("id");
    $("#" + selected + "-content").show();
});

function activateTab(tabId) {
    $(".code-tab li").removeClass("is-active");
    $("#" + tabId).addClass("is-active");

    $(".code-tab-content").hide();
    $("#" + tabId + "-content").show();
}
