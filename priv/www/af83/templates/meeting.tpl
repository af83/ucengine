<div class="page" id="meeting">
    <section class="large">
        <div class="block info">
            <div class="block-content">
                <p><strong>Meeting's name :</strong> <span>{{meeting_name}}</span></p>
                <!--<p><strong><span>{{meeting_users}}</span> connected users</strong></p>-->
                <p><strong>Description :</strong><span>{{meeting_desc}}</span></p>
                <p class="quit"><a href="#/meeting/{{meeting_name}}/quit">Quit the meeting</a></p>
            </div>
        </div>

        <div class="block tools">
            <div class="block-content files" id="files"></div>
        </div>

    </section>

    <section class="main">
        <article id="chat">
            <div class="block-content" id="chat_content"></div>
        </article>

        <article id="video">
            <div class="block-header">
                <h2>Video</h2>
            </div>
            <div class="block-content">
                <div id="video_player"></div>
            </div>
        </article>

        <article id="whiteboard">
            <div class="block-header">
                <h2>Whiteboard</h2>
            </div>
            <div id="whiteboard_content" class="block-content"></div>
        </article>

        <div id="wheel">
            <p><img src="images/wheel.png" /></p>
        </div>

    </section>
</div>
