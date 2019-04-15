import * as React from 'react';
import { Table } from 'react-bootstrap';

interface TableProps {
  header: String[]
  body: String[][]
}
export const ItemTable = (props: TableProps) => (
    <Table responsive bordered hover size="sm">
      <thead>
        <tr>
          <th></th>
          {props.header.map((h: string, i: number) => <th key={i}>{h}</th>)}
        </tr>
      </thead>
      <tbody>
        {props.body.map((b: string[], i: number) => {
           return (
             <tr key={i}>
               <th>{i+1}</th>
               {b.map((bc: string, i2: number) => <td key={i2}>{bc}</td>)}
             </tr>
           )
        })}
      </tbody>
    </Table>
)
