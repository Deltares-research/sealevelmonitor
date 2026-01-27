This README provides instructions on how to use the `sidebar-footer.html` and `header.html` files in the `_quarto.yml` configuration file.

## Adding a custom header to the page
To add a custom header to your HTML pages, add the following code to your _quarto.yml file:

```yml
format:
  html:
    include-before-body: custom-component/header.html
```

## Adding a custom footer to the page
To add a custom footer to your HTML pages, add the following code to your _quarto.yml file:

```yml
book:
  body-footer: custom-component/header.html
```

## Customizing the page top header
You can customize the page top header by changing the background color. To do so, add the following code to your _quarto.yml file:

```yml
book:
  navbar: 
    background: "#2E89BF"
```
## Customizing the page bottom footer
In the page footer, you can add your own logo or text. To do so, add the following code to your _quarto.yml file:

```yml
book:
  page-footer: 
    background: "#2E89BF"
    center: |
      [![](assets/deltares_logo_blauw.svg){fig-alt="deltares" width=100px}](https://www.deltares.nl/en)
      [![](assets/ministery_logo.png){fig-alt="ministery-of-infrastructure" width=100px}](https://www.rijksoverheid.nl/ministeries/ministerie-van-infrastructuur-en-waterstaat)
```

## Adding information to the sidebar footer
To add information to the sidebar footer, add the following code to your _quarto.yml file:

```yml
book:
  sidebar:
   footer: custom-component/sidebar-footer.html
```
