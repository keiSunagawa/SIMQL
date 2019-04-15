import * as React from 'react';
import Highlight from 'react-highlight'
import AceEditor from 'react-ace';
import './syntax/mode.js'
import 'brace/theme/idle_fingers';
import '../../node_modules/highlight.js/styles/agate.css';
import * as R from 'ramda'

import { SIMQL } from '../scala/playground-fastopt'
import { Button, Form } from 'react-bootstrap';

import { ItemTable } from '../components/table'
// import 'brace/mode/json';

interface SimqlState {
  simqlQuery: string
  predef: string
  userdef: string
  error: String
  sql: String
  header: String[]
  body: String[][]
}
export class SimqlApp extends React.Component<{}, SimqlState> {
  appHandler: any

  constructor(props: {}) {
    super(props)
    // initialize state
    this.state = {
      simqlQuery: INITSQL,
      predef: PREDEF,
      userdef: USERDEF,
      error: "",
      sql: "",
      header: [],
      body: []
    }

    this.appHandler = SIMQL.compile(
      // presenter functions
      () => this.state.simqlQuery, // getSimqlQuery
      () => this.state.predef, // getPredef
      () => this.state.userdef, // getUserdef
      (sql: String) => { // printSQL
        this.setState({ sql })
      },
      (error: String) => { console.log(error) }, // printError
      (sql: String) => { // sendSQL
        console.log(sql)
      }
    )

    this.handleClick = this.handleClick.bind(this)
    this.handleQureyForm = this.handleQureyForm.bind(this)
    this.handleUserdefForm = this.handleUserdefForm.bind(this)
  }

  handleClick() {
    this.appHandler.submit()
  }

  handleQureyForm(value: string) {
    this.setState({ simqlQuery: value })
  }
  handleUserdefForm(value: string) {
    this.setState({ userdef: value })
  }

  render() {
    return (
      <div className="sipp-contemt">
        <h2>SIMQL</h2>
        <Form>
          <AceEditor
            mode="simql"
            theme="idle_fingers"
            height="100px"
            width="800px"
            enableBasicAutocompletion={true}
            enableLiveAutocompletion={true}
            value={this.state.simqlQuery}
            onChange={this.handleQureyForm}
            name="queryInput"
            editorProps={{$blockScrolling: true}}
          />
        </Form>
        <div className="sipp-contemt">
          <Button
            variant="primary"
            onClick={this.handleClick}
          >Submit</Button>
        </div>
        <SQL code={this.state.sql} />
        <details>
          <summary>Predef(readOnly)</summary>
          <AceEditor
            mode="fimql"
            readOnly={true}
            theme="idle_fingers"
            height="500px"
            width="800px"
            enableBasicAutocompletion={true}
            enableLiveAutocompletion={true}
            value={this.state.predef}
            name="predefInput"
            editorProps={{$blockScrolling: true}}
          />
        </details>
        <span>Userdef</span>
        <AceEditor
          mode="fimql"
          theme="idle_fingers"
          height="300px"
          width="800px"
          tabSize={2}
          enableBasicAutocompletion={true}
          enableLiveAutocompletion={true}
          value={this.state.userdef}
          onChange={this.handleUserdefForm}
          name="userInput"
          editorProps={{$blockScrolling: true}}
        />
      </div>
    )
  }
}

interface SQLProps {
  code: String
}
const SQL = (props: SQLProps) => (
  <div>
    <Highlight className='sql'>{props.code}</Highlight>
  </div>
)

const PREDEF = `define {
  defun is_null(lhs: Expr) => Expr {
    q{ $\`? IS NULL\`($lhs) }
  }
  defun is_not_null(lhs: Expr) => Expr {
    q{ $\`? IS NOT NULL\`($lhs) }
  }
  defun table_access(col: Symbol, index: Number) => Symbol {
    let tbl = $get_table($index)
    let accessor = $csm($tbl, $single_dot)
    $csm($accessor, $col)
  }
  defun c(col: Expr) => Raw {
    q{ $\`COUNT(?)\`($col) }
  }
  defun like(col: Symbol, keyword: String) => Expr {
    let lk = $cst($keyword, "%")
    let lk = $cst("%", $lk)
    q{ $\`? LIKE(?)\`($col, $keyword) }
  }
  defun identity<A>(a: A) => A {
    $a
  }
  defun reverse<E>(xs: List<E>) => List<E> {
    let id = (xs2: List<E>, next: E) => List<E> {
      $cons($next, $xs2)
    }
    let init = nil<E>
    $fold($xs, $init, $id)
  }
}`
const USERDEF = `define {
  defun succ(x: Number) => Number {
    $add($x, 1)
  }
}`
const INITSQL = `dual ?> $succ(1)`
