<nav>
    <ol>
        <li>Admin</li>
        <li>{{org}}</li>
    </ol>
</nav>
<section>
    <div class="block mettings">
        <div class="block-header">
            <h2>Meetings</h2>
        </div>
        <div class="block-content">
            <ul>
                {{#meetings}}
                <li>
                    <p><a href="#/admin/org/{{org}}/meeting/{{name}}">{{name}}</a></p>
                </li>
                {{/meetings}}
            </ul>
        </div>
    </div>
    <div class="block mettings">
        <div class="block-header">
            <h2>New meeting</h2>
        </div>
        <form method="post" action="#/admin/org/{{org}}/meeting"> 
           <fieldset>
                <label>Name</label>
                <input type="text" name="name" /><br />
                <label>Description</label>
                <input type="text" name="description" /><br />
                <label>Start date</label>
                <input type="datetime" name="start" /><br />
                <label>End date</label>
                <input type="datetime" name="end" /><br />
                <input type="submit" value="Create" />
            </fieldset>
        </form>
    </div>
</section>
