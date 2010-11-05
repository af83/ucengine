<div class="page">
<section class="large">
  <div class="block register">
    <div class="block-header">
      <h1>Create an account</h1>
    </div>
    <div class="block-content">
      {{#has_error}}
      <ul class="errors">
	{{#errors}}
	<li>&bull; {{.}}</li>
	{{/errors}}
      </ul>
      {{/has_error}}

      <form method="post" action="#/register">
      
	<p>
          <label for="email">Email <span>*</span></label>
          <input type="text" id="email" name="email" />
	</p>
      
	<p>
          <label for="pseudo">Pseudo <span>*</span></label>
          <input type="text" id="pseudo" name="nickname" />
	</p>
      
	<p>
          <label for="company">Company</label>
          <input type="text" id="company" name="company" />
	</p>
	
	<p>
          <label for="pwd1">Password <span>*</span></label>
          <input type="password" id="pwd1" name="pwd1" />
	</p>
	<p>
          <label for="pwd2">Confirm password <span>*</span></label>
          <input type="password" id="pwd2" name="pwd2" />
	</p>
	
	<p class='checkbox'>
          <input type="checkbox" id="terms" name="terms" />
          <label for="terms">I accept the <a href="#">terms and conditions</a> of UCengine</label>
	</p>
      
	<ul>
          <li><input type="reset" value="Reset" /></li>
          <li><input type="submit" value="Register" /></li>
	</ul>
      </form>
    </div>
  </div>
</section>
</div>
