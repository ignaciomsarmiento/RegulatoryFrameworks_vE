# ============================
# UI sources for each page
# ============================


source("tabs/ui/guide.R", local = TRUE)
source("tabs/ui/about.R", local = TRUE)
source("tabs/ui/forthcoming.R", local = TRUE) 

# ============================
# MAIN UI
# ============================

shinyUI(
    fluidPage(
    shinyjs::useShinyjs(),
    
    # ---- HEAD ----
    tags$head(
      tags$title("Regulatory Frameworks Explorer"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;600&family=Source+Sans+Pro:wght@300;400;600&display=swap",
        rel = "stylesheet"
      ),
      includeCSS("www/styles.css"),
      
      # ---- JAVASCRIPT TO CONTROL TABS ----
      tags$script(HTML("
        Shiny.addCustomMessageHandler('trigger-download', function(id) {
          const el = document.getElementById(id);
          if (el) el.click();
        });
      ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
    
        // Function to update active state
        function updateActiveNav(activeTab) {
          // Remove active class from all nav links
          document.querySelectorAll('.nav-link').forEach(function(link) {
            link.classList.remove('active');
          });
          
          // Add active class to the current tab
          const activeLink = document.querySelector('.nav-link[data-tab=\"' + activeTab + '\"]');
          if (activeLink) {
            activeLink.classList.add('active');
          }
        }
    
        // Set initial active state (landing page)
        updateActiveNav('landing');
    
        // Attach click handler to each header nav link
        document.querySelectorAll('.nav-link').forEach(function(link) {
          link.addEventListener('click', function(e) {
            e.preventDefault();
    
            // Get the tab name from data-tab attribute
            let tab = this.getAttribute('data-tab');
    
            // Update active state immediately
            updateActiveNav(tab);
    
            // Find the hidden Shiny tab button that matches this name
            let tabButton = document.querySelector(
              'a[data-value=\"' + tab + '\"]'
            );
    
            // Simulate a click to switch the tab
            if (tabButton) {
              tabButton.click();
            }
          });
        });
    
        // Listen for tab changes (in case tabs are changed programmatically)
        const observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.attributeName === 'class') {
              const tabs = document.querySelectorAll('#main_tabs > .tab-pane');
              tabs.forEach(function(tab) {
                if (tab.classList.contains('active')) {
                  const tabValue = tab.getAttribute('data-value');
                  if (tabValue) {
                    updateActiveNav(tabValue);
                  }
                }
              });
            }
          });
        });
    
        // Observe tab changes
        const tabPanes = document.querySelectorAll('#main_tabs > .tab-pane');
        tabPanes.forEach(function(pane) {
          observer.observe(pane, { attributes: true });
        });
    
      });
    ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        const TAB_PARAM = 'tab';
        const tabContainer = document.getElementById('main_tabs');
        let isHistoryNav = false;
        let currentTab = null;

        function getActiveTab() {
          if (!tabContainer) return null;
          const active = tabContainer.querySelector('.tab-pane.active');
          return active ? active.getAttribute('data-value') : null;
        }

        function syncUrl(tab, replace) {
          if (!tab) return;
          const url = new URL(window.location);
          url.searchParams.set(TAB_PARAM, tab);
          const method = replace ? 'replaceState' : 'pushState';
          window.history[method]({ tab: tab }, '', url);
        }

        function switchToTab(tab, fromPop) {
          if (!tab) return;
          if (fromPop) isHistoryNav = true;
          if (tab === currentTab) {
            if (fromPop) isHistoryNav = false;
            return;
          }
          const btn = document.querySelector('a[data-value=\"' + tab + '\"]');
          if (btn) btn.click();
        }

        // Hook Bootstrap tab events (Shiny uses BS tabs under the hood)
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
            const tab = window.jQuery(e.target).data('value');
            if (!tab) return;
            if (isHistoryNav) {
              syncUrl(tab, true); // keep URL in sync without adding history on back/forward
              isHistoryNav = false;
            } else if (tab !== currentTab) {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
        }

        if (tabContainer) {
          const observer = new MutationObserver(function() {
            const tab = getActiveTab();
            if (!tab) return;
            if (tab === currentTab) {
              if (isHistoryNav) isHistoryNav = false;
              return;
            }
            if (isHistoryNav) {
              syncUrl(tab, true);
              isHistoryNav = false;
            } else {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
          tabContainer.querySelectorAll('.tab-pane').forEach(function(pane) {
            observer.observe(pane, { attributes: true, attributeFilter: ['class'] });
          });
        }

        const initialTab = new URL(window.location).searchParams.get(TAB_PARAM) || getActiveTab() || 'landing';
        syncUrl(initialTab, true);
        isHistoryNav = true;
        switchToTab(initialTab, true);
        setTimeout(function() { isHistoryNav = false; }, 0);
        currentTab = initialTab;

        window.addEventListener('popstate', function(event) {
          const tabFromState = event.state && event.state.tab;
          const tabFromUrl = new URL(window.location).searchParams.get(TAB_PARAM);
          switchToTab(tabFromState || tabFromUrl || 'landing', true);
        });
      });
    "))
    ),
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function () {
    const btn = document.querySelector('.hamburger-btn');
    const menu = document.querySelector('.hamburger-dropdown');

    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      menu.classList.toggle('hidden');
    });

    document.addEventListener('click', function () {
      menu.classList.add('hidden');
    });
  });
")),
    

    # ---- HEADER ----
    tags$div(
      class = "header",
      tags$div(
        class = "header-content",
        
        # Logo (izquierda)
        tags$img(
          src = "WB.png",
          class = "wb-logo",
          style = "cursor: pointer;",
          onclick = "document.querySelector('a[data-value=\"landing\"]').click();"
        ),
        
        # Spacer (centro) — mantiene alineación del grid
        tags$div(),
        
        # Hamburger (derecha)
        tags$div(
          class = "hamburger-menu",
          
          # Botón ☰
          tags$div(
            class = "hamburger-btn",
            HTML("&#9776;")  # ☰
          ),
          
          # Dropdown
          tags$div(
            class = "hamburger-dropdown hidden",
            
            tags$a(
              "Home",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"landing\"]').click();"
            ),
           
            
            tags$hr(),
            
            tags$a(
              "Non-Salary Labor Costs",
              class = "nav-link",
              onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'})"
            ),
            tags$a(
              "Minimum Wages",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();"
            ),
            tags$a(
              "Business Taxes",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();"
            ),
            tags$hr(),
            tags$a(
              "Guide",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"Guide\"]').click();"
            ),
            tags$a(
              "About",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"About\"]').click();"
            ),
            
          )
        )
      )
    ),
    
    # ---- MAIN BODY ----
    div(
      class = "main-content",
      tabsetPanel(
        id = "main_tabs",
        type="hidden",
        selected = "landing",
        
        # ============================
        # 1. LANDING PAGE
        # ============================
        tabPanel(
          "landing",
          tags$div(
            class = "landing-new",
            
            # Hero
            tags$div(
              class = "landing-hero",
              tags$div(
                class = "landing-hero-image",
                style = "background-image: url('landing2.jpeg');"
              ),
              tags$div(
                class = "landing-hero-text",
                tags$div(
                  class = "landing-hero-heading",
                  tags$div(
                    class = "landing-hero-eyebrow-col",
                    tags$span(class = "landing-eyebrow", "WELCOME TO THE")
                  ),
                  tags$div(
                    class = "landing-hero-title-col",
                    h1(class = "landing-hero-title", "Regulatory", tags$br(), "Frameworks Explorer")
                  )
                ),
                p(
                  class = "landing-hero-desc",
                  "Explore comprehensive data on ",
                  tags$a(
                    href = "#",
                    class = "landing-inline-link",
                    onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'}); return false;",
                    "non-salary labor costs"
                  ),
                  ", ",
                  tags$a(
                    href = "#",
                    class = "landing-inline-link",
                    onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click(); return false;",
                    "minimum wages"
                  ),
                  " and ",
                  tags$a(
                    href = "#",
                    class = "landing-inline-link",
                    onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click(); return false;",
                    "business taxes"
                  ),
                  " across countries. Dive into interactive visualizations and detailed analyses to understand regional regulatory frameworks."
                )
              )
            ),
            
            # Topics
            tags$div(
              class = "landing-section",
              tags$span(class = "landing-section-label", "Choose a regulatory framework topic to explore detailed data and visualizations"),
              tags$div(
                class = "landing-card-grid",
                
                tags$a(
                  href = "#",
                  class = "landing-card",
                  onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'}); return false;",
                  tags$div(class = "topic-card-image labor-img"),
                  tags$div(
                    class = "landing-card-body",
                    h3("Non-Salary Labor Costs"),
                    p("Explore non-wage labor costs")
                  )
                ),
                
                tags$a(
                  href = "#",
                  class = "landing-card",
                  onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click(); return false;",
                  tags$div(class = "topic-card-image minwage-img"),
                  tags$div(
                    class = "landing-card-body",
                    h3("Minimum wages"),
                    p("Explore minimum wage levels across countries")
                  )
                ),
                
                tags$a(
                  href = "#",
                  class = "landing-card",
                  onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click(); return false;",
                  tags$div(class = "topic-card-image btax-img"),
                  tags$div(
                    class = "landing-card-body",
                    h3("Business Taxes"),
                    p("Explore business tax rates and regulations")
                  )
                )
              )
            ),
            
            # Explore more
            tags$div(
              class = "landing-section explore-more",
              tags$span(class = "landing-section-label", "Explore more"),
              tags$div(
                class = "landing-card-grid small",
                
                tags$a(
                  href = "#",
                  class = "landing-card",
                  onclick = "document.querySelector('a[data-value=\"Guide\"]').click(); return false;",
                  tags$div(
                    class = "landing-card-body",
                    h3("Guide"),
                    p("Learn how to read and navigate the data")
                  )
                ),
                
                tags$a(
                  href = "#",
                  class = "landing-card",
                  onclick = "document.querySelector('a[data-value=\"About\"]').click(); return false;",
                  tags$div(
                    class = "landing-card-body",
                    h3("About"),
                    p("Meet the team and project goals")
                  )
                )
              )
            ),
            
            tags$div(
              class = "footer",
              tags$p(class = "footer-text", "© 2026 World Bank Group")
            )
          )
        ),
        
        # ============================
        # 2. Guide 
        # ============================
        tabPanel("Guide", guide), 
        
        # ============================
        # 3. About
        # ============================
        tabPanel(
          "About", about
        ),
        # En la sección de tabsetPanel, después de "About"
        tabPanel(
          "forthcoming",
          forthcoming
        ),
        # ============================
        # 4. CONTENT MODULE PAGE
        # ============================
        tabPanel(
          "content",
          div(
            class = "content-area",
            uiOutput("dynamic_content")
          ),
          tags$div(
            class = "footer",
            tags$p(class = "footer-text", "© 2026 World Bank Group")
          )
        )
      )
    )
  )
)
