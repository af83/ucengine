<div class="page" id="meeting">

  <ul id="dock">
    <li id="chat-dock" />
    <li id="video-dock" />
    <li id="fileupload-dock" />
    <li id="filesharing-dock" />
    <li id="whiteboard-dock" />
  </ul>

  <div id="widgets">
    <section id="infos">
      <article class="block info">
        <div class="block-content">
          <p><strong>Meeting name :</strong> <span>{{meeting_name}}</span></p>
          <p><strong>Description :</strong><span>{{meeting_desc}}</span></p>
          <p class="quit"><a href="#/meeting/{{meeting_id}}/quit">Quit the meeting</a></p>
        </div>
      </article>
      <article id="timer" />
    </section>

    <section id="expanded" class="slots">
      <article id="video" />
    </section>

    <section id="reduced" class="slots">
      <article id="fileupload" />
      <article id="filesharing" />
      <article id="chat" />
      <article id="whiteboard" />
    </section>

    <section id="replay-mode">
      <div id="replay"></div>
      <div class="toggle-results"></div>
      <div id="search"></div>

      <div id="search-results">
        <div class="ui-search-title">Search results</div>
        <div id="activity"></div>
        <div id="results"></div>
      </div>
    </section>
  </div>
</div>
