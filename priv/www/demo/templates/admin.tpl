<div class="page">
    <section>
        <h1>Organisations</h1>
        <div class="block org">
            <div class="block-header">
                <h2>Listing</h2>
            </div>
            <div class="block-content">
                <ul>
                    {{#orgs}}
                    <li>
                        <p><a href="#/admin/org/{{name}}">{{name}}</a></p>
                    </li>
                    {{/orgs}}
                </ul>
            </div>
        </div>
        <div class="block org">
            <div class="block-header">
                <h2>New organisation</h2>
            </div>
            <div class="block-content">
                <form method="post" action="#/admin/org">
                    <fieldset>
                        <label>Name</label>
                        <input type="text" name="name" />
                        <input type="submit" value="Create" />
                    </fieldset>
                </form>
            </div>
        </div>
    </section>
    <section>
        <h1>Users</h1>
        <div class="block user">
            <div class="block-header">
                <h2>Listing</h2>
            </div>
            <div class="block-content">
                <ul>
                    {{#users}}
                    <li>
                        <p><a href="#/admin/user/{{uid}}">{{uid}}</a></p>
                    </li>
                    {{/users}}
                </ul>
            </div>
        </div>
        <div class="block user">
            <div class="block-header">
                <h2>New user</h2>
            </div>
            <div class="block-content">
                <form method="post" action="#/admin/user">
                    <fieldset>
                        <label>Name</label>
                        <input type="text" name="name" /><br /> <!-- TODO: learn HTML -->
                        <label>Password</label>
                        <input type="password" name="password" /><br />
                        <label>Nickname</label>
                        <input type="text" name="nickname" /><br />
                        <input type="submit" value="Create" />
                    </fieldset>
                </form>
            </div>
        </div>

    </section>
</div>
