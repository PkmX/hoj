var React         = require('react'),
    AceEditor     = require('react-ace'),
    Semantify     = require('react-semantify'),
    request       = require('superagent'),
    ansi_up       = require('ansi_up'),
    escapeHtml    = require('./escapeHtml'),
    ga            = require('react-google-analytics'),
    jQuery        = require('imports?jQuery=jquery!exports?jQuery!semantic-ui/dist/semantic.js'),
    $             = jQuery,
    GAInitializer = ga.Initializer,
    Button        = Semantify.Button,
    Checkbox      = Semantify.Checkbox,
    Column        = Semantify.Column,
    Field         = Semantify.Field,
    Form          = Semantify.Form,
    Grid          = Semantify.Grid,
    Icon          = Semantify.Icon,
    Input         = Semantify.Input,
    Item          = Semantify.Item,
    Label         = Semantify.Label,
    Menu          = Semantify.Menu,
    Modal         = Semantify.Modal,
    Segment       = Semantify.Segment;

require('colors');
require('semantic-ui/dist/semantic.min.css');
require('./style.scss');

var ProblemModal = React.createClass({

    getDefaultProps: function() {
        return {
            className: 'problem-modal',
            pdf: 'dummy.pdf'
        };
    },

    render: function() {
        return(
            <Modal className={this.props.className + " long fullscreen"} init={true}>
                <i className="close icon"></i>
                <div className="header">
                    Problem Title
                </div>
                <div className="content">
                    <div className="description">
                        <embed width="100%" height="600" name="plugin" src={this.props.pdf} type="application/pdf" />
                    </div>
                </div>
                <div className="actions">
                    <div className="ui button">OK</div>
                </div>
            </Modal>
        );
    }
});

var Panel = React.createClass({

    getInitialState: function(){
        return {
            cases         : [],
            message       : '',
            output        : '',
            output_eof_nl : true,
            input         : localStorage.getItem('input')      || '',
            local         : localStorage.getItem('local') == 1 || false,
            code          : localStorage.getItem('code')       || '',
            float         : localStorage.getItem('float')      || 'left'
        };
    },

    formatConsoleOutput: function(output) {
        if (output)
            return ansi_up.ansi_to_html(escapeHtml(output)).replace(/(?:\r\n|\r|\n)/g, '<br />');
        else
            return '';
    },

    handleChange: function(name, e) {
        var state = this.state;

        switch(name)
        {
            case 'code':
                state.code = e;
                localStorage.setItem('code', e);
                break;
            case 'local':
                state.local = e.target.checked;
                localStorage.setItem('local', state.local ? 1 : 0);
                break;
            case 'input':
                state.input = e.target.value;
                localStorage.setItem('input', state.input);
                break;
            case 'float':
                state.float = e.target.checked ? 'right' : 'left';
                localStorage.setItem('float', state.float);
                break;
        }

        this.setState(state);
        //this.code = code;
        //localStorage.setItem('code', code);
    },

    updateSegments: function(body) {
        var state = this.state;
        state.message =  this.formatConsoleOutput(body.compilerOutput);
        state.message += this.formatConsoleOutput(body.compilerError);
        state.cases   =  body.results || [];
        state.output  =  this.formatConsoleOutput(body.output);

        if (state.output !== '' && ! body.output.match(/(\r\n|\r|\n)$/g)) {
            state.output_eof_nl = false;
        } else {
            state.output_eof_nl = true;
        }

        this.setState(state);
    },

    handleSubmit: function() {
        var btn = this.refs.submit.getDOMNode();
        var state = this.state;

        $(btn).addClass('loading').removeClass('blue');

        var req = request
            .post(window.location.origin + '/submit')
            .field('code', this.state.code)
            .field('local', this.state.local);

        if (this.state.local)
            req.field('input', this.state.input);

        req.end(function(err, res) {
            if (res.ok) {
                this.updateSegments(res.body);
            } else {
                state.cases = [];
                state.message = 'server error';
                this.setState(state);
            }
            $(btn).removeClass('loading').addClass('blue');
        }.bind(this));

        return false;
    },

    handleModal: function(target, e) {
        window.open('problem.pdf', '_blank', 'menubar=no, location=no, titlebar=no');
    },

    handleResize: function() {
        this.setState(this.state);
    },

    componentDidMount: function(){
        var self = this;
        $('.ui.checkbox').checkbox().each(function(i, e){
            $(e).change(self.handleChange.bind(self, $(e).children('input').attr('name')));
        });

        window.addEventListener('resize', this.handleResize);

        // send request to Google Analytics
        ga('create', 'UA-44646112-4', 'auto');
        ga('send', 'pageview');
    },

    render: function() {

        var aceHeight = $(window).height() * 0.7;

        var items = React.Children.map(this.state.cases, function(item) {
            var color = 'green';

            if(item.toLowerCase() === 'correct') {
                color = 'green';
            } else {
                color = 'red';
            }

            return (<li style={{color: color}}>{item}</li>);
        });

        var compilerMessagePanel = [
            <Label className="hoj offset-top">Compiler Message</Label>,
            <Segment className="inverted" style={{overflowX: 'auto'}}>
                <code dangerouslySetInnerHTML={{__html: this.state.message}}></code>
            </Segment>
        ];

        var ioPanel;

        if (this.state.local) {
            ioPanel = [
                <Label className="hoj offset-top">Input</Label>,
                <Form className="hoj offset-top">
                    <div className="field">
                        <textarea className="hoj input" onChange={this.handleChange.bind(this, 'input')} value={this.state.input} />
                    </div>
                </Form>,
                <Label className="hoj offset-top">Output</Label>,
                <Segment>
                    <code dangerouslySetInnerHTML={{__html: this.state.output}}></code>
                </Segment>
            ];

            if (this.state.output_eof_nl === false) {
                ioPanel.push(
                    <div><i className="warning sign icon"></i> No newline at end of file</div>
                );
            }
        } else {
            ioPanel = [
                <Label className="hoj offset-top">Test Cases</Label>,
                <Segment>
                    <ol className="cases">
                        {items}
                    </ol>
                </Segment>
            ];
        }

        return (
            <Grid className="three column centered grid">
                <Column className=""><h1 style={{marginTop: 20}} className="ui header centered">Haskell Online Judge</h1></Column>
                <Column className="fifteen wide centered row">
                    <Column className="six wide" style={{float: this.state.float}}>
                        <Form>
                            <div className="four fields">
                                <Field style={{paddingTop: '0.5rem'}}>
                                    <Checkbox className="toggle">
                                        <input type="checkbox" name="float" checked={this.state.float === 'right'} onChange={function(){}} />
                                        <label>Float</label>
                                    </Checkbox>
                                </Field>
                                <Field style={{paddingTop: '0.5rem'}}>
                                    <Checkbox className="toggle">
                                        <input type="checkbox" name="local" checked={this.state.local} onChange={function(){}} />
                                        <label>Local</label>
                                    </Checkbox>
                                </Field>
                                <Field>
                                    <Button onClick={this.handleModal.bind(this, '.problem-modal')}><i className="file text icon"></i> Problem</Button>
                                </Field>
                            </div>
                        </Form>
                        {ioPanel}
                        {compilerMessagePanel}
                    </Column>
                    <Column className="ten wide">
                        <Form>
                            <Field>
                                <Label className="hoj offset-top">Code</Label>
                                <div className="hoj offset-top"></div>
                                <AceEditor
                                    mode="c_cpp"
                                    theme="monokai"
                                    onChange={this.handleChange.bind(this, 'code')}
                                    name="code"
                                    value={this.state.code}
                                    width="100%"
                                    height={aceHeight + 'px'}
                                />
                            </Field>
                            <Field style={{textAlign: 'right'}}>
                                <Button className="green right" onClick={this.handleSubmit} ref="submit"><i className="check icon"></i> Submit</Button>
                            </Field>
                        </Form>
                    </Column>
                </Column>
            </Grid>
        );
    }
});

React.render(
    <div>
        <Panel />
        <GAInitializer />
    </div>,
    document.body
);

