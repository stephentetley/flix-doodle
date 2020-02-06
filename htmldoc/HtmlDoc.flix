/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

enum HtmlElement {
    case HtmlText(String),
    case HtmlTag(String, List[HtmlAttr], Html)
}

enum HtmlAttr {
    case HtmlAttr(String, String)
}

enum Html {
    case Html(List[HtmlElement])
}
    
namespace HtmlDoc {
    
    pub def primText(s: String): HtmlElement = HtmlText(s)

    pub def attr(name: String, value: String): HtmlAttr = HtmlAttr(name, value)

    pub def tag(name: String, attrs: List[HtmlAttr], content: Html): HtmlElement = 
        HtmlTag(name, attrs, content)

    pub def append(x: Html, y: Html): Html = match (x, y) {
        case (Html(xs), Html(ys)) => Html(List.append(xs, ys))
    }

    pub def body(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("body", attrs, content)

    pub def h1(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h1", attrs, content)
    pub def h2(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h2", attrs, content)
    pub def h3(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h3", attrs, content)
    pub def h4(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h4", attrs, content)
    pub def h5(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h5", attrs, content)
    pub def h6(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("h6", attrs, content)

    pub def table(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("table", attrs, content)

    pub def tr(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("tr", attrs, content)

    pub def td(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("td", attrs, content)
 
    pub def ol(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("ol", attrs, content)
    pub def ul(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("ul", attrs, content)
    pub def li(attrs: List[HtmlAttr], content: Html): HtmlElement = tag("li", attrs, content)
    
}