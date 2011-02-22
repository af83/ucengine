<div class="page">
    <section class="large">
        <div class="block welcome">
            <div class="block-header">
                <h1>{{welcome}}</h1>
            </div>
            <div class="block-content">
                <p>{{description}}</p>
            </div>
            {{#not_connected}}
            <div class="block-footer">
                <p><a href="#/register">&gt;&gt; Register now</a></p>
            </div>
            {{/not_connected}}
        </div>

        {{#not_connected}}
        <div class="block">
            <div class="block-header">
                <h2>Sign in</h2>
            </div>
            <div class="block-content">
                <form method="post" action="#/user/login">
                    <p>
                        <label>Email <span>*</span></label>
                        <input type="text" name="email"/>
                    </p>
                    <p>
                        <label>Password <span>*</span></label>
                        <input type="password" name="password"/>
                    </p>
                    <ul>
                        <li><input type="submit" value="connection" /></li>
                        <li>or <a href="#/register">Register</a></li>
                    </ul>
                </form>
            </div>
        </div>
        {{/not_connected}}
        {{^not_connected}}
        <div class="block tweets">
            <div class="block-header">
                <img src="images/image.png" alt="ucengine" /> U.C.Engine
            </div>
            <div class="block-content">
                <ul id="lastTweets">

                </ul>
            </div>
            <div class="block-footer">
                <a href="http://twitter.com/ucengine"><img src="images/twitter.png" alt="Twitter" /></a>
            </div>
        </div>
        {{/not_connected}}
    </section>

    <section class="main">
        <div class="block events">
            <div class="block-header">
                <h2>Current events</h2>
            </div>
            <div class="block-content">
                <ul id="currentMeetings">
                    {{#currentmeetings}}
                    <li>
                        <p><strong>{{name}}</strong></p>
                        <p>{{#format}}{{start_date}}{{/format}}</p>
                        <p>{{metadatas.description}}</p>
                        {{^not_connected}}
                        <p class="go"><a href="#/meeting/{{name}}">JOIN</a></p>
                        {{/not_connected}}
                    </li>
                    {{/currentmeetings}}
                    {{^currentmeetings}}
                     <li>no meetings</li>
                    {{/currentmeetings}}
                </ul>
            </div>
            <div class="block-footer">
            </div>
        </div>

        <div class="block events">
            <div class="block-header">
                <h2>Last events</h2>
            </div>
            <div class="block-content">
                <ul id="closedMeetings">
                    {{#closedmeetings}}
                    <li>
                        <p><strong>{{name}}</strong></p>
                        <p>{{#format}}{{start_date}}{{/format}}</p>
                        <p>{{metadatas.description}}</p>
                        {{^not_connected}}
                        <p class="go"><a href="#/meeting/{{name}}">Replay</a></p>
                        {{/not_connected}}
                    </li>
                    {{/closedmeetings}}
                    {{^closedmeetings}}
                     <li>no meetings</li>
                    {{/closedmeetings}}
                </ul>
            </div>
            <div class="block-footer">
            </div>
        </div>

        <div class="block events last">
            <div class="block-header">
                <h2>Upcoming events</h2>
            </div>
            <div class="block-content">
                <ul id="upcomingMeetings">
                    {{#upcomingmeetings}}
                    <li>
                        <p><strong>{{name}}</strong></p>
                        <p>{{#format}}{{start_date}}{{/format}}</p>
                        <p>{{metadatas.description}}</p>
                        {{^not_connected}}
                        <p class="go"><a href="#/meeting/{{name}}">JOIN</a></p>
                        {{/not_connected}}
                    </li>
                    {{/upcomingmeetings}}
                    {{^upcomingmeetings}}
                     <li>no meetings</li>
                    {{/upcomingmeetings}}
                </ul>
            </div>
            <div class="block-footer">
            </div>
        </div>
    </section>
</div>
