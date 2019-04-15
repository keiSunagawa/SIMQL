import * as React from 'react';

import { Form } from 'react-bootstrap';

interface TextInputProps {
  id: string
  label: string
  handler: (value: string) => void
  placeholder?: string
}
export const TextInput = (props: TextInputProps) => (
  <Form.Group controlId={props.id}>
    <Form.Label>{props.label}</Form.Label>
    <Form.Control type="text" placeholder={props.placeholder} onChange={(e: any) => props.handler(e.target.value)}/>
  </Form.Group>
)

export const TextAreaInput = (props: TextInputProps) => (
  <Form.Group controlId={props.id}>
    <Form.Label>{props.label}</Form.Label>
    <Form.Control
      as="textarea"
      type="text"
      placeholder={props.placeholder}
      onChange={(e: any) => props.handler(e.target.value)}/>
  </Form.Group>
)

interface FileInputProps {
  id: string
  label: string
  handler: (file: File) => void
}
export const FileInput = (props: FileInputProps) => (
  <Form.Group controlId={props.id}>
    <Form.Label>{props.label}</Form.Label>
    <Form.Control type="file" onChange={(e: any) => {
      const f = e.target.files.item(0)
      props.handler(f)
    }}/>
  </Form.Group>
)
