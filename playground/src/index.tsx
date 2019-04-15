import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Route, Link, HashRouter } from 'react-router-dom';

import { Container, ButtonGroup, Button } from 'react-bootstrap';
import { LinkContainer } from 'react-router-bootstrap';

import { SimqlApp } from './simql/SimqlApp'

const Root = () => (
  <Container>
    <HashRouter>
      <div>
        <Route exact path="/" component={SimqlApp} />
      </div>
    </HashRouter>
  </Container>
);

ReactDOM.render(
  <div>
    <Root />
  </div>,
    document.getElementById('root')
);
